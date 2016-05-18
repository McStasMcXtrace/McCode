#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Tools for piping a process to the terminal for std I/O.
'''
from subprocess import Popen, PIPE, STDOUT
import re
import argparse
from threading import Thread

def cleanTrace(data):
    ''' 
    splits data into three sections: 
    
    instrument definition
    component draw calls 
    neutron rays
    
    and captures all other text in a tertiary string
    '''
    pos_instr = data.find('INSTRUMENT:\n')
    pos_mcdisplay = data.find('MCDISPLAY: start')
    pos_neutrons = data.find('ENTER:\n')
    
    
    try:
        # get instrument definition
        lines = data[pos_instr:pos_mcdisplay].splitlines()
        cont = True
        lidx = 2
        while cont:
            testline = lines[lidx]
            if re.match('COMPONENT:', testline):
                lidx += 3
            else:
                cont = False
        instrdeftext = ''
        for i in range(lidx):
            instrdeftext = instrdeftext + lines[i] + '\n'
        remainder = ''
        for line in lines[lidx:]:
            remainder = remainder + line + '\n'
        
        # get mcdisplay draw calls
        lines = data[pos_mcdisplay:pos_neutrons].splitlines()
        cont = True
        lidx = 0
        while cont:
            if re.match('MCDISPLAY:', lines[lidx]):
                lidx += 1
            else:
                cont = False
        mcdisplaytext = ''
        for i in range(lidx):
            mcdisplaytext = mcdisplaytext + lines[i] + '\n'
        for line in lines[lidx+1:]: # NOTE: the +1 is because of the line "INSTRUMENT END:"
            remainder = remainder + line + '\n'
        
        # get neutron ray section (with trailing comment lines)
        lines = data[pos_neutrons:].splitlines()
        
        # filter datalines
        raylines = []
        raycomments = []
        problem_idxs = []
        
        for i in range(len(lines)):
            mat = re.match('(\w+):', lines[i])
            if mat and mat.group(1) in ['ENTER', 'COMP', 'STATE', 'SCATTER', 'ABSORB', 'LEAVE']:
                
                # theck for corrupted lines 
                seapos = lines[i].find('Warning: ')
                if seapos > 0:
                    # in this case, the whole neutron ray should be removed. Save the comment and the index.
                    raycomments.append(lines[i][seapos:])
                    problem_idxs.append(i)
                    continue
                
                raylines.append(lines[i])
            else:
                raycomments.append(lines[i])
        
        # Handle the problems identified above; raylines interrupted by "Warning: " somewhere in the middle. 
        # We must exterminate those rays from the data.
        lineslen = len(raylines)
        for idx in problem_idxs:
            # search backwards for "ENTER":
            i = -1
            idx_enter = -1
            idx_leave = -1
            while True:
                i += 1
                
                # find last ENTER:
                if idx_enter == -1 and re.match('ENTER:', raylines[idx-i]):
                    idx_enter = idx-i
                
                if idx_leave == -1 and idx+i == lineslen:
                    idx_leave = lineslen-1
                elif idx_leave == -1 and re.match('LEAVE:', raylines[idx+i]):
                    idx_leave = idx+i
                
                if idx_enter >= 0 and idx_leave >= 0:
                    for j in range(idx_enter, idx_leave+1): # NOTE: there should always be a STATE after LEAVE
                        del raylines[idx_enter] # NOTE: raylines will disappear underway
                    break
        
        # make sure the sequence ends with a LEAVE then a STATE
        while True:
            lineslen = len(raylines)
            last = raylines[lineslen-1]
            nextlast = raylines[lineslen-2]
            if (not re.match('LEAVE:', nextlast)) or (not re.match('STATE:', last)):
                del raylines[lineslen-1]
            else:
                break
        
        # reconstruct strings from lists
        raystext = ''
        for line in raylines:
            raystext = raystext + line + '\n'
        for line in raycomments:
            remainder = remainder + line + '\n'
        
        return instrdeftext, mcdisplaytext, raystext, remainder
    
    except Exception as e:
        print e.message

class LineFilter(object):
    pass

class LineBuffer(object):
    '''
    Fixed size FIFO buffer with no overflow warning.
    Thread-safe due to atomic variable checks (idx_w and idx_r).
    '''
    buffer = None
    maxlines = -1
    idx_r = -1
    idx_w = -1
    
    def __init__(self, maxlines):
        self.buffer = [None] * maxlines
        self.maxlines = maxlines
        self.idx_r = 0
        self.idx_w = 0
    
    def add_line(self, line):
        ''' add a line to the buffer (no overflow check '''
        self.buffer[self.idx_w % self.maxlines] = line
        self.idx_w += 1
    
    def read_line(self):
        ''' read the next line in the buffer '''
        r = self.idx_r
        w = self.idx_w
        if not r > w:
            self.idx_r += 1
            return self.buffer[r % self.maxlines]
        else:
            return None
    
    def read_all_lines(self):
        ''' read all new lines, returning a list '''
        r = self.idx_r
        w = self.idx_w
        lines = []
        while not r == w:
            lines.append(self.buffer[r % self.maxlines])
            r += 1
        self.idx_r = r
        return lines
    
    def read_all_lines_str(self):
        ''' read all new lines, returning them as a single string '''
        r = self.idx_r
        w = self.idx_w
        data = ''
        while not r == w:
            data = data + self.buffer[r % self.maxlines]
            r += 1
        self.idx_r = r
        return data

class McrunPipeThread(Thread):
    ''' 
    Thread object which handles PIPE I/O. 
    Switches to live buffer writing when printend string is matched in an output line.
    '''
    cmd = ''
    instrbuffer = None
    neutronbuffer = None
    instrdef_start = ''
    neutrondef_start = ''
    prompt_phase = None
    neutron_phase = None
    
    def __init__(self, cmd, instrdef_start, neutrondef_start):
        ''' constructor '''
        self.cmd = cmd
        self.instrbuffer = LineBuffer(maxlines=1000)
        self.neutronbuffer = LineBuffer(maxlines=10000)
        self.instrdef_start = instrdef_start
        self.neutrondef_start = neutrondef_start
        self.prompt_phase = True
        self.neutron_phase = False
        Thread.__init__(self)
    
    def run(self):
        ''' create a process given command and read, print and write to it depending on state '''
        process = Popen(self.cmd, shell=True,
                        stdout=PIPE,
                        stderr=STDOUT,
                        stdin=PIPE
                        )
        
        while process.poll() == None:
            stdoutdata = process.stdout.readline()
            
            if self.prompt_phase or not self.neutron_phase:
                if re.search('\]:', stdoutdata):
                    data = raw_input()
                    process.stdin.write(data + '\n')
                if re.match(self.instrdef_start, stdoutdata):
                    self.prompt_phase = False
                if re.match(self.neutrondef_start, stdoutdata):
                    self.neutron_phase = True
                
            self.print_or_save(stdoutdata)
        
        # empty process buffer 
        for stdoutdata in process.stdout:
            self.print_or_save(stdoutdata)
        
    def print_or_save(self, line):
        ''' adds lines to print, instbuffer or neutronbuffer depending on state '''
        if self.prompt_phase:
            print line.rstrip('\n')
        elif self.neutron_phase:
            self.neutronbuffer.add_line(line)
        else: 
            self.instrbuffer.add_line(line)

class McrunPipeMan(object):
    ''' 
    Proxy class for setting up the McrunPipeThread thread and LineBuffer. 
    These are intended to provide parallel processing and piping, due to 
    potentially long simulation execution times.
    '''
    cmd = ''
    instrdef_start = ''
    instrdef_end = ''
    
    def __init__(self, cmd):
        self.cmd = cmd
        self.thread = McrunPipeThread(cmd=cmd, instrdef_start=r'INSTRUMENT:\n', neutrondef_start='ENTER:\n')
    
    def start_pipe(self):
        self.thread.start()
    
    def join(self):
        self.thread.join()

    def read_neutrons(self):
        return self.thread.neutronbuffer.read_all_lines_str()
    
    def read_instrdef(self):
        return self.thread.instrbuffer.read_all_lines_str()

def main(args):
    pipeman = McrunPipeMan('mcrun ESS_Brilliance_2013.instr --trace -n1')
    pipeman.start_pipe()
    pipeman.join()
    data = pipeman.read_instrdef()
    print 
    print "McrunPipeMan collected trace output:"
    print
    print data

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('--test', action='store_true', help='')
    args = parser.parse_args()
    main(args)

