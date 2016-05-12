#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Tools for piping a process to the terminal for std I/O.

NOTE: Due to limitations inherent in piping (the requirement for 
intelligent evalueation of stdout to determine the difference 
between prompt and idle), this code is mcrun specific. 
A search for ']:' is done in the implementation of McrunPipeThread.
'''
from subprocess import Popen, PIPE, STDOUT
import re
import argparse
from threading import Thread

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
        self.buffer[self.idx_w % self.maxlines] = line
        self.idx_w += 1
    
    def read_line(self):
        r = self.idx_r
        w = self.idx_w
        if not r > w:
            self.idx_r += 1
            return self.buffer[r % self.maxlines]
        else:
            return None
    
    def read_all_lines(self):
        r = self.idx_r
        w = self.idx_w
        lines = []
        while not r == w:
            lines.append(self.buffer[r % self.maxlines])
            r += 1
        self.idx_r = r
        return lines

class McrunPipeThread(Thread):
    ''' 
    Thread object which handles PIPE I/O. 
    Switches to live buffer writing when printend string is matched in an output line.
    '''
    linebuffer = None
    cmd = ''
    printend = ''
    prompt_phase = True
    
    def __init__(self, cmd, linebuffer, printend):
        self.cmd = cmd
        self.linebuffer = linebuffer
        self.printend = printend
        self.prompt_phase = True
        Thread.__init__(self)
    
    def run(self):
        process = Popen(self.cmd, shell=True,
                        stdout=PIPE,
                        stderr=STDOUT,
                        stdin=PIPE
                        )
        
        while process.poll() == None:
            stdoutdata = process.stdout.readline()
            
            if self.prompt_phase:
                if re.search('\]:', stdoutdata):
                    data = raw_input()
                    process.stdin.write(data + '\n')
                if re.match(self.printend, stdoutdata):
                    self.prompt_phase = False
                
            self.print_or_save(stdoutdata)
    
    def print_or_save(self, line):
        if self.prompt_phase:
            print line.rstrip('\n')
        else: 
            self.linebuffer.add_line(line)

class McrunPipeMan(object):
    ''' 
    Proxy class for setting up the McrunPipeThread thread and LineBuffer. 
    These are intended to provide parallel processing and piping, due to 
    potentially long simulation execution times.
    '''
    linebuffer = None
    cmd = ''
    instrdef_start = ''
    instrdef_end = ''
    
    def __init__(self, cmd):
        self.cmd = cmd
        self.linebuffer = LineBuffer(maxlines=10000)
        self.thread = McrunPipeThread(cmd=cmd, linebuffer=self.linebuffer, printend=r'INSTRUMENT:\n')
    
    def start_pipe(self):
        self.thread.start()
    
    def join(self):
        self.thread.join()

    def readall(self):
        data = ''
        for line in self.linebuffer.read_all_lines():
            data = data + line
        return data

def main(args):
    pipeman = McrunPipeMan('mcrun ESS_Brilliance_2013.instr --trace -n1')
    pipeman.start_pipe()
    pipeman.join()
    data = pipeman.readall()
    print 
    print "McrunPipeMan collected trace output:"
    print
    print data

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('--test', action='store_true', help='')
    args = parser.parse_args()
    main(args)

