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
        return ''.join(self.instrdef) + '\n'
    
    def get_neutrons(self):
        self.neutronsdone.wait()
        return ''.join(self.neutron_blocks) + '\n'

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
        super(InstrState, self).__init__(setcurrent, next, databox, args)
        
    def add_line(self, line):
        if self.first:
            self.databox.add_instrdef(line)
            self.first = False
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
        allstates['prompt'] = PromptState(setcurrent, next=allstates['instr'], databox=databox, args={'use_defaultpars': True})
        
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

class McrunPipeMan_NEW(object):
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

##################################
# older versions below this line #
##################################

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
        raystext = '\n'.join(raylines) + '\n'
        remainder = '\n'.join(raycomments) + '\n'
        
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
        # TODO: could be improved performance-wise by the use of '\n'.join()+'\n'
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
    
    send_enter = None
    
    prompt_phase = None
    neutron_phase = None
    
    instrdef_finished = None
    
    def __init__(self, cmd, instrdef_start, neutrondef_start, inspect=None, send_enter=False):
        ''' constructor '''
        self.cmd = cmd
        self.instrbuffer = LineBuffer(maxlines=5000)
        
        if inspect:
            self.neutronbuffer = FilterBufferInspect(maxlines=50000, inspect_compname=inspect)
        else:
            self.neutronbuffer = LineBuffer(maxlines=50000)
        
        self.instrdef_start = instrdef_start
        self.neutrondef_start = neutrondef_start
        
        self.send_enter = send_enter
        
        self.prompt_phase = True
        self.neutron_phase = False
        self.instrdef_finished = Event()
        self.instrdef_finished.clear()
        
        Thread.__init__(self)
    
    def run(self):
        ''' create a process given command and read, print and write to it depending on state '''
        process = Popen(self.cmd, shell=True,
                        stdout=PIPE,
                        stderr=None,
                        stdin=PIPE
                        )
        
        while process.poll() == None:
            stdoutdata = process.stdout.readline()
            
            if self.prompt_phase or not self.neutron_phase:
                if re.search('\]:', stdoutdata):
                    if not self.send_enter:
                        process.stdin.write(raw_input() + '\n')
                    else:
                        process.stdin.write('\n')
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
    
    def __init__(self, cmd, inspect=None, send_enter=False):
        self.cmd = cmd
        self.thread = McrunPipeThread(cmd=cmd, instrdef_start=r'INSTRUMENT:\n', neutrondef_start='ENTER:\n', inspect=inspect, send_enter=send_enter)
    
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

