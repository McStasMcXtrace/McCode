#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Tools for piping a process to the terminal for std I/O.
Buffering and low-level filtering. Thread management.
'''
from subprocess import Popen, PIPE, STDOUT
import re
import argparse
from threading import Thread, Event

class TraceDataCleaner(object):
    pass

def cleanTrace(data):
    ''' 
    splits data into three sections: 
    
    instrument definition
    component draw calls 
    neutron rays
    
    and captures all other text in a tertiary string
    '''
    if data == '':
        return
    
    pos_instr = data.find('INSTRUMENT:\n')
    pos_mcdisplay = data.find('MCDISPLAY: start')
    pos_neutrons = data.find('ENTER:\n')
    
    try:
        remainder = ''
        instrdeftext = ''
        mcdisplaytext = ''
        
        # instrument definition section
        if pos_instr >= 0:
        
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
            
            for i in range(lidx):
                instrdeftext = instrdeftext + lines[i] + '\n'
            
            for line in lines[lidx:]:
                remainder = remainder + line + '\n'
            
            # get mcdisplay draw calls
            lines = data[pos_mcdisplay:pos_neutrons].splitlines()
            cont = True
            lidx = 0
            while cont:
                if re.match('MCDISPLAY:', lines[lidx]):
                    lidx += 1
                    if lidx == len(lines):
                        cont = False
                else:
                    cont = False
            
            for i in range(lidx):
                mcdisplaytext = mcdisplaytext + lines[i] + '\n'
            if not lidx == len(lines):
                for line in lines[lidx+1:]: # NOTE: the +1 is because of the line "INSTRUMENT END:"
                    remainder = remainder + line + '\n'
            
        if pos_neutrons == -1:
            return instrdeftext, mcdisplaytext, '', remainder
        
        #
        # get neutron rays section (with trailing comment lines)
        #
        lines = data[pos_neutrons:].splitlines()
        
        # filter datalines
        raylines = []
        raycomments = []
        problem_idxs = []
        
        for i in range(len(lines)):
            mat = re.match('(\w+):', lines[i])
            if mat and mat.group(1) in ['ENTER', 'COMP', 'STATE', 'SCATTER', 'ABSORB', 'LEAVE']:
                
                # check for corrupted lines 
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
            if idx >= lineslen:
                continue
            
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
                        del raylines[idx_enter] # NOTE: raylines will disappear during process
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

#class FilterBufferInspect(LineBuffer):
class FilterBufferInspect(object):
    '''
    An extension of LineBuffer implementing --inspect by adding a extra 
    buffer layer.
    '''
    linebuffer = None
    mini = None
    
    reachedcomp = False
    lastcallwasleave = False
    isblockend = False
    inspect_compname = ''
    def __init__(self, maxlines, inspect_compname):
        self.linebuffer = LineBuffer(maxlines)
        self.mini = []
        
        self.isblockend = False
        self.reachedcomp = False
        self.lastcallwasleave = False
        self.inspect_compname = inspect_compname
        #super(FilterBufferInspect, self).__init__(maxlines)
    
    def add_line(self, line):
        '''
        Adds lines to minibufer until a leave (plus one line) is encountered.
        '''
        if not self.reachedcomp:
            self.reachedcomp = bool(re.match('COMP: "%s"' % self.inspect_compname, line))
        if self.lastcallwasleave:
            self.lastcallwasleave = False
            self.isblockend = True
        if not self.isblockend and not self.lastcallwasleave:
            self.lastcallwasleave = bool(re.match('LEAVE:', line))
            
        if not self.isblockend:
            self.mini.append(line)
        else:
            self.isblockend = False
            if self.reachedcomp:
                self.reachedcomp = False
                self.mini.append(line)
                for l in self.mini:
                    self.linebuffer.add_line(l)
            # clear this cache
            self.mini = []
    
    def read_line(self):
        return self.linebuffer.read_line()
    
    def read_all_lines_str(self):
        ''' python inheritance overrides all methods... '''
        return self.linebuffer.read_all_lines_str()

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
    instrdef_finished = None
    
    def __init__(self, cmd, instrdef_start, neutrondef_start, inspect=None):
        ''' constructor '''
        self.cmd = cmd
        self.instrbuffer = LineBuffer(maxlines=1000)
        
        if inspect:
            self.neutronbuffer = FilterBufferInspect(maxlines=30000, inspect_compname=inspect)
        else:
            self.neutronbuffer = LineBuffer(maxlines=30000)
        
        self.instrdef_start = instrdef_start
        self.neutrondef_start = neutrondef_start
        
        self.prompt_phase = True
        self.neutron_phase = False
        self.instrdef_finished = Event()
        self.instrdef_finished.clear()
        
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
                    self.instrdef_finished.set()
                
            self.print_or_save(stdoutdata)
        
        # empty process buffer 
        for stdoutdata in process.stdout:
            self.print_or_save(stdoutdata)
        
    def join_instrdef(self):
        ''' returns when the instrument definition has been read '''
        self.instrdef_finished.wait()
    
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
    
    def __init__(self, cmd, inspect=None):
        self.cmd = cmd
        self.thread = McrunPipeThread(cmd=cmd, instrdef_start=r'INSTRUMENT:\n', neutrondef_start='ENTER:\n', inspect=inspect)
    
    def start_pipe(self):
        self.thread.start()
    
    def join(self):
        self.thread.join()

    def join_instrdef(self):
        self.thread.join_instrdef()

    def read_neutrons(self):
        return self.thread.neutronbuffer.read_all_lines_str()
    
    def read_instrdef(self):
        return self.thread.instrbuffer.read_all_lines_str()

