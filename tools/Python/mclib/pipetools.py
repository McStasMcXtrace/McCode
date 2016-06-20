'''
Tools for piping a process to the terminal for std I/O.
Buffering and low-level filtering. Thread management.
'''
from subprocess import Popen, PIPE
import re
from threading import Thread, Event

class DataBox():
    ''' container for pre-processed trace data '''
    def __init__(self):
        self.comments = []
        self.instrdef = []
        self.neutron_blocks = []
        
        # event objects
        self.instrdone = Event()
        self.instrdone.clear()
        self.neutronsdone = Event()
        self.neutronsdone.clear()
    
    def add_comment(self, line):
        self.comments.append(line)
    
    def add_instrdef(self, line):
        self.instrdef.append(line)
    
    def add_neutronblock(self, block):
        self.neutron_blocks.append(block)

    def set_instrdone(self):
        self.instrdone.set()
    
    def set_neutronsdone(self):
        self.neutronsdone.set()
    
    def get_instrdef(self):
        self.instrdone.wait()
        return ''.join(self.instrdef)
    
    def get_neutrons(self):
        self.neutronsdone.wait()
        return ''.join(self.neutron_blocks)

    def get_comments(self):
        self.instrdone.wait()
        self.neutronsdone.wait()
        return ''.join(self.comments)
        
class LineHandlerState(object):
    def __init__(self, setcurrent, next, databox, args=None):
        '''
        Abstract state for all line handler states - instrdef, neutrons, etc.
            setcurrent: a fuction of state, line
            allstates: a dictionary of all states given typename
            databox: global data destination / buffer
        '''
        self.setcurrent = setcurrent
        self.next = next
        self.databox = databox
        self.args = args
        
    def add_line(self, line):
        ''' override to implement a state '''
        pass

class PromptState(LineHandlerState):
    def add_line(self, line):
        # prompt case
        if re.search('\]:', line):
            if not self.args['use_defaultpars']:
                self.process.stdin.write(raw_input() + '\n')
            else:
                self.process.stdin.write('\n')
        # switch case
        if re.match(r'INSTRUMENT:\n', line):
            self.setcurrent(self.next, line)
            return
        
        print(line)
        self.databox.add_comment(line)
    
    def setprocess(self, process):
        self.process = process

class InstrState(LineHandlerState):
    def __init__(self, setcurrent, next, databox, args=None):
        self.idx = 0
        self.first = True
        self.second = False
        super(InstrState, self).__init__(setcurrent, next, databox, args)
        
    def add_line(self, line):
        if self.first:
            self.databox.add_instrdef(line)
            self.first = False
            self.second = True
        elif self.second:
            self.databox.add_instrdef(line)
            self.second = False
        elif (self.idx % 3) is not 0 or re.match(r'COMPONENT: ', line):
            self.databox.add_instrdef(line)
            self.idx += 1
        elif re.match('MCDISPLAY: start\n', line):
            self.setcurrent(self.next, line)
        else:
            self.databox.add_comment(line)

class McdisplayState(LineHandlerState):
    def add_line(self, line):
        if re.match('MCDISPLAY: ', line):
            self.databox.add_instrdef(line)
        elif re.match('INSTRUMENT END:\n', line):
            self.databox.add_instrdef(line)
        elif re.match('ENTER:\n', line):
            self.databox.set_instrdone()
            self.setcurrent(self.next, line)
        else:
            self.databox.add_comment(line)

class NeutronsState(LineHandlerState):
    def __init__(self, setcurrent, next, databox, args=None):
        self.block = []
        self.active = False
        self.leaveflag = False
        
        self.inspect = args.get('inspect', None)
        super(NeutronsState, self).__init__(setcurrent, next, databox, args)
    
    def add_line(self, line):
        if re.match('LEAVE:', line):
            self.block.append(line)
            self.leaveflag = True
            self.active = False
        elif self.active: 
            self.block.append(line)
        elif self.leaveflag:
            self.block.append(line)
            
            # inspect
            accept_block = False
            if self.inspect:
                for b in self.block:
                    if self.inspect in b:
                        accept_block = True
            else:
                accept_block = True
            if accept_block: 
                self.databox.add_neutronblock(''.join(self.block))
            
            self.block = []
            self.leaveflag = False
        elif re.match('ENTER:', line):
            self.block.append(line)
            self.active = True
        else:
            self.databox.add_comment(line)

class TraceReader(Thread):
    def _setcurrent(self, current, line):
        self.current = current
        current.add_line(line)
    
    def __init__(self, cmd, inspect=None, use_defaultpars=False):
        # set up state machine
        setcurrent = lambda current, line: self._setcurrent(current, line)
        allstates = {}
        databox = DataBox()
        
        allstates['neutrons'] = NeutronsState(setcurrent, next=None, databox=databox, args={'inspect': inspect})
        allstates['mcdisplay'] = McdisplayState(setcurrent, next=allstates['neutrons'], databox=databox)
        allstates['instr'] = InstrState(setcurrent, next=allstates['mcdisplay'], databox=databox)
        allstates['prompt'] = PromptState(setcurrent, next=allstates['instr'], databox=databox, args={'use_defaultpars': use_defaultpars})
        
        # remember
        self.current = allstates['prompt']
        self.allstates = allstates
        self.databox = databox
        self.cmd = cmd
        
        Thread.__init__(self)
    
    def run(self):
        ''' create a process given command and read, print and write to it depending on state '''
        process = Popen(self.cmd, shell=True,
                        stdout=PIPE,
                        stderr=None,
                        stdin=PIPE
                        )
        
        # a special case: give prompt state access to the process if default_pars are to be used
        self.allstates['prompt'].setprocess(process)
        
        while process.poll() == None:
            stdoutdata = process.stdout.readline()
            self.current.add_line(stdoutdata)
        
        # empty process buffer
        for stdoutdata in process.stdout:
            self.current.add_line(stdoutdata)
        
        self.databox.set_neutronsdone()

class McrunPipeMan(object):
    '''
    Proxy class for setting up the McrunPipeThread thread and LineBuffer.
    These are intended to provide parallel processing and piping, due to 
    potentially long simulation execution times.
    '''
    cmd = ''
    instrdef_start = ''
    instrdef_end = ''
    
    def __init__(self, cmd, inspect=None, send_enter=False):
        self.cmd = cmd
        self.reader = TraceReader(cmd=cmd, inspect=inspect, use_defaultpars=send_enter)
    
    def start_pipe(self):
        self.reader.start()
    
    def join(self):
        self.reader.join()

    def read_neutrons(self):
        return self.reader.databox.get_neutrons()
    
    def read_instrdef(self):
        return self.reader.databox.get_instrdef()

    def read_comments(self):
        return self.reader.databox.get_comments()
