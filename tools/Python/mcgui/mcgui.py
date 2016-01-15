#!/usr/bin/env python
'''
mgcui program 

Initial version written during Winter/spring of 2015

@author: jaga
'''
import sys
import os
import webbrowser
import subprocess
import time
import re
import mccode_config
from PyQt4 import QtGui, QtCore
from viewclasses import McView
from mcguiutils import McGuiUtils
from mcfileutils import McComponentParser
from datetime import datetime

''' Message emitter
Status and message log and signalling.
'''
class McMessageEmitter(QtCore.QObject):
    statusUpdate = QtCore.pyqtSignal(QtCore.QString)
    __statusLog = []
    
    logMessageUpdate = QtCore.pyqtSignal(QtCore.QString, bool)
    __msgLog = []
    
    def status(self, status):
        ''' status / current general state info
        '''
        if status == None:
            return
        
        self.statusUpdate.emit(status)
        self.__statusLog.append(status)
        QtGui.QApplication.processEvents()
    
    def message(self, msg, err_msg=False):
        ''' message log messages (simulation progress etc.)
        '''
        if msg == None:
            return
        
        self.logMessageUpdate.emit(msg, err_msg)
        self.__msgLog.append(msg)
        QtGui.QApplication.processEvents()


''' Asynchronous process execution QThread
'''        
class McRunQThread(QtCore.QThread):
    thread_exception = QtCore.pyqtSignal(QtCore.QString)
    error = QtCore.pyqtSignal(QtCore.QString)
    message = QtCore.pyqtSignal(QtCore.QString)
    cmd = ''
    process_returncode = None
    
    def run(self, *args, **kwargs):
        try:
            # check cmd is set
            if self.cmd == '':
                raise Exception('McRunQThread: Set cmd before running Start()')
            
            # open a subprocess with shell=True, otherwise stdout will be buffered and thus 
            # not readable live
            process = subprocess.Popen(self.cmd, 
                                       stdout=subprocess.PIPE, 
                                       stderr=subprocess.PIPE,
                                       shell=True)
            
            # read program output while the process is active
            while process.poll() == None:
                stdoutdata = process.stdout.readline().rstrip('\n')
                self.message.emit(stdoutdata)
                stderrdata = process.stderr.readline().rstrip('\n')
                self.error.emit(stderrdata)
                time.sleep(0.05)
            # flush until EOF
            for stdoutdata in process.stdout:
                self.message.emit(stdoutdata.rstrip('\n'))
            for stderrdata in process.stderr:
                self.error.emit(stderrdata.rstrip('\n'))
            
            self.process_returncode = process.returncode
                        
        except:
            (type, value, traceback) = sys.exc_info()
            self.thread_exception.emit(value.message)
            

''' State
Holds unique state values and mediates some low-level actions.
'''
class McGuiState(QtCore.QObject):
    __instrFile = ''
    __emitter = None
    
    # <instrument>, <work dir>
    instrumentUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    # [<canRun>, <canPlot>] each can be str 'True' or 'False'
    simStateUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    
    __cFile = ""
    __binaryFile = ""
    __resultFile = ""
    __dataDir = ""
    
    def __init__(self, emitter):
        super(McGuiState, self).__init__()
        self.setWorkDir(os.getcwd())
        self.__emitter = emitter
        
    def __fireInstrUpdate(self):
        self.instrumentUpdated.emit([self.getInstrumentFile(), self.getWorkDir()])
        
    def __fireSimStateUpdate(self):
        self.simStateUpdated.emit([str(self.canRun()), str(self.canPlot()), str(self.isSimRunning())])
    
    def init(self):
        ''' must be called after construction to emit events used for sync'ing to initial state '''
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
    
    def getInstrumentFile(self):
        return self.__instrFile
    
    def loadInstrument(self, instr_file):
        # makes sure this is not a qstring
        instr_file = str(instr_file)
        # file must exists and be .instr file:
        if os.path.exists(instr_file) and (os.path.splitext(instr_file)[1] == '.instr'):
            # handle .instr files loaded without full path
            if os.path.dirname(instr_file) != '':
                realdir = os.path.dirname(os.path.abspath(instr_file))
                print(realdir)
                self.setWorkDir(realdir)
                
            instr_file = os.path.join(self.getWorkDir(), os.path.basename(instr_file))
            self.__instrFile = instr_file
            self.__fireInstrUpdate()
            self.__emitter.status("Instrument: " + os.path.basename(instr_file))
            self.__fireSimStateUpdate()
        else:
            # TODO: throw exception
            self.__emitter.status('Could not load file: ' + instr_file)
            #raise Exception("McGuiState.loadInstrument: Error. File: " + instr_file)
        
    def unloadInstrument(self):
        self.__instrFile = ''
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
        return True
    
    def saveInstrumentIfFileExists(self, text):
        instr = self.getInstrumentFile()
        if not os.path.exists(instr):
            return False
        McGuiUtils.saveInstrumentFile(instr, text)
        return True
    
    def getWorkDir(self):
        return os.getcwd()

    def getDataDir(self):
        return self.__dataDir
    
    def setWorkDir(self, newdir):
        if not os.path.isdir(newdir):
            raise Exception('McGuiState.setWorkDir: invalid work dir - "' + newdir + '"')
        os.chdir(newdir)
    
    def canCompile(self):
        return self.__instrFile != ""
    
    def canRun(self):
        return self.__instrFile != ""
    
    def canPlot(self):
        return ((self.__instrFile != "") and (not self.isSimRunning()))
    
    __thread_exc_signal = QtCore.pyqtSignal(QtCore.QString)
    def compile(self, mpi=False):
        # using Qt in-built cross-thread signaling
        self.__thread_exc_signal.connect(handleExceptionMsg)
        
        # clear any existing .c and .out files
        if self.__binaryFile:
            if os.path.isfile(self.__binaryFile):
                os.remove(self.__binaryFile)
                self.__binaryFile = ''
        if self.__cFile:
            if os.path.isfile(self.__cFile):
                os.remove(self.__cFile)
                self.__cFile = ''
        
        # compile simulation in a background thread
        self.compilethread = QtCore.QThread()
        self.compilethread.run = lambda: self.compileAsync(mpi, self.__thread_exc_signal)
        self.compilethread.finished.connect(lambda: self.__emitter.message('compile thread done \n'))
        self.compilethread.start()
        
    def compileAsync(self, mpi, thread_exc_signal):
        try:
            # generate mcstas .c file from instrument
            nf = self.__instrFile
            cmd = mccode_config.configuration["MCCODE"] + ' -t '  + nf
            process = subprocess.Popen(cmd, 
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       shell=True)
            self.__emitter.status('Compiling instrument to c ...')
            self.__emitter.message('Compiling instrument to c ...')
            self.__emitter.message(cmd)
            
            # read program output while the process is active
            while process.poll() == None:
                stdoutdata = process.stdout.readline().rstrip('\n')
                self.__emitter.message(stdoutdata)
                stderrdata = process.stderr.readline().rstrip('\n')
                self.__emitter.message(stderrdata, err_msg=True)
                time.sleep(0.05)
            ## flush until EOF
            for stdoutdata in process.stdout:
                self.__emitter.message(stdoutdata.rstrip('\n'))
            for stderrdata in process.stderr:
                self.__emitter.message(stderrdata.rstrip('\n'), err_msg=True)
                
            # paths and filenames
            spl = os.path.splitext(os.path.basename(str(nf)))
            basef = os.path.join(self.getWorkDir(), spl[0])
            cf = basef + '.c'
            
            # check
            if os.path.isfile(cf):
                self.__cFile = cf
                self.__emitter.message('    --> ' + self.__cFile)
            else:
                raise Exception('C file not found')
            
            # look for CFLAGS in the generated C code
            cflags = mccode_config.compilation["CFLAGS"] 
            ccode = open(self.__cFile)
            for line in ccode:
                line = line.rstrip()
                if re.search('CFLAGS=', line) :
                    label, flags = line.split('=', 1)
                    cflags = cflags + ' ' + flags
            
            # compile binary from mcstas .c file 
            bf = basef + '.' + mccode_config.platform["EXESUFFIX"] 
            if mpi:
                cmd = mccode_config.compilation["MPICC"] + ' -o ' + bf + ' ' + cf + ' ' + cflags + ' ' + mccode_config.compilation["MPIFLAGS"]
            else:
                cmd = mccode_config.compilation["CC"] + ' -o ' + bf + ' ' + cf + ' ' + cflags
           
            process = subprocess.Popen(cmd, 
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       shell=True)
            self.__emitter.status('Compiling instrument to binary ...')
            self.__emitter.message('Compiling instrument to binary ...')
            self.__emitter.message(cmd)
    
            # read program output while the process is active
            while process.poll() == None:
                stdoutdata = process.stdout.readline().rstrip('\n')
                self.__emitter.message(stdoutdata)
                stderrdata = process.stderr.readline().rstrip('\n')
                self.__emitter.message(stderrdata, err_msg=True)
                time.sleep(0.05)
            # flush until EOF
            for stdoutdata in process.stdout:
                self.__emitter.message(stdoutdata.rstrip('\n'))
            for stderrdata in process.stderr:
                self.__emitter.message(stderrdata.rstrip('\n'), err_msg=True)
                    
            # check
            if os.path.isfile(bf):
                self.__binaryFile = bf
                self.__emitter.message('    --> ' + self.__binaryFile)
                self.__emitter.status('Instrument compiled')
            else:
                raise Exception('compileAsync: Binary not found.')
            
            self.__fireSimStateUpdate()
        
        except: 
            self.__emitter.status("")

            (type, value, traceback) = sys.exc_info()
            if thread_exc_signal:
                thread_exc_signal.emit(value.message)
            else:
                raise
    
    __runthread = None
    def isSimRunning(self):
        if self.__runthread != None:
            return self.__runthread.isRunning()
        else:
            return False
    
    def interrupt(self):
        # interrupt any running simulation
        if self.__runthread:
            if self.__runthread.isRunning():
                self.__runthread.terminate()
        else:
            raise Exception('State.interrupt: no __runthread')
    
    def run(self, fixed_params, params):
        ''' fixed_params[]:
                0 - simulation = 0, trace = 1
                1 - neutron count (int)
                2 - steps count (int)
                3 - gravity (bool)
                4 - clustering 0/1/2 (single/MPI/MPIrecompile) (int)
                5 - clustering # nodes (int)
                6 - random seed (int)
                7 - output directory (str)
            params[]:
                [<par_name>, <value>] pairs
        '''
        
        mcrunparms = ' '
        # assemble mpi-related options
        clustering = fixed_params[4]
        if clustering == 1:
            mcrunparms = ' --mpi=' + fixed_params[5] + ' '
        elif clustering == 2:
            mcrunparms = ' -c --mpi=' + fixed_params[5] + ' '
        
        # sim/trace and output directory
        simtrace = fixed_params[0]
        if simtrace == 0:
            output_dir = str(fixed_params[7])
            if output_dir == '':
                DATE_FORMAT_PATH = "%Y%d%m_%H%M%S"
                output_dir = "%s_%s" % \
                              (self.__instrFile,
                               datetime.strftime(datetime.now(), DATE_FORMAT_PATH))
                
            runstr = mccode_config.configuration["MCRUN"] + mcrunparms + self.__instrFile + ' -d ' + output_dir
            self.__dataDir = output_dir
        else:
            runstr = mccode_config.configuration["MCDISPLAY"] + ' ' + self.__instrFile + ' --no-output-files '
            self.__dataDir = "None"
        
        # neutron count
        ncount = fixed_params[1]
        if int(ncount) > 0:
            runstr = runstr + ' -n ' + str(ncount)
        
        # steps count
        nsteps = fixed_params[2]
        if nsteps != '':
            if int(nsteps) > 1:
                print 'Nsteps is ' + nsteps
                runstr = runstr + ' -N ' + str(nsteps)
        
        # gravity
        if fixed_params[3]:
            gravity = True
            runstr = runstr + ' --gravity '
        
        # random seed
        random_seed = fixed_params[6]
        if (random_seed):
            if int(random_seed) > 0:
                runstr = runstr + ' -s ' + str(random_seed)
        
        # parse instrument params
        for p in params:
            runstr = runstr + ' ' + p[0] + '=' + p[1]
        
        print('Running: '+runstr)

        # Ensure assembled runstr is a string - QStrings breaks the runAsync execution!
        runstr = str(runstr)
        
        # run simulation in a background thread
        self.__runthread = McRunQThread()
        self.__runthread.cmd = runstr
        self.__runthread.finished.connect(lambda: self.__runFinished(self.__runthread.process_returncode))
        self.__runthread.terminated.connect(self.__runTerminated)
        self.__runthread.thread_exception.connect(handleExceptionMsg)
        self.__runthread.error.connect(lambda msg: self.__emitter.message(msg, err_msg=True))
        self.__runthread.message.connect(lambda msg: self.__emitter.message(msg))
        self.__runthread.start()
        
        self.__emitter.message(runstr)
        self.__emitter.status('Running simulation ...')
        self.__fireSimStateUpdate()
        
    def __runFinished(self, process_returncode):
        if not self.__interrupted:
            self.__fireSimStateUpdate()
            if process_returncode == 0:
                self.__emitter.message('simulation done')
            self.__emitter.message('')
            self.__emitter.status('')
        else: 
            self.__fireSimStateUpdate()
            self.__emitter.message('simulation interrupted')
            self.__emitter.message('')
            self.__emitter.status('Simulation interrupted')
        self.__interrupted = False
    
    __interrupted = False
    def __runTerminated(self):
        self.__interrupted = True
    
    def getInstrParams(self):
        # get instrument params using 'mcrun [instr] --info'
        # returns: params: a list of [name, value] pairs 
        cmd = mccode_config.configuration["MCRUN"] + ' ' + self.__instrFile + " --info"
        process = subprocess.Popen(cmd, 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.PIPE,
                                   shell=True)
        # synchronous call
        (stdoutdata, stderrdata) = process.communicate()
        # note: communicate() always sets a process exit code
        
        if stderrdata:
            self.__emitter.message(stderrdata, err_msg=True)
            
        if process.returncode != 0:
            raise Exception('Instrument compile error.')
        
        # get parameters from info
        params = []
        for l in stdoutdata.splitlines():
            if 'Param:' in l:
                s = l.split()[1]
                s = s.split('=')
                params.append(s)
                
        return params


''' Controller
Implements ui and data callbacks.
'''
class McGuiAppController():
    def __init__(self):
        self.emitter = McMessageEmitter()
        self.view = McView()
        self.state = McGuiState(self.emitter)
    
        self.connectCallbacks()
        self.initDynamicView()
        
        self.state.init()
        
        # load instrument file from command line pars
        for a in sys.argv:
            if os.path.isfile(a):
                if os.path.splitext(a)[1] == '.instr':
                    if self.state.getInstrumentFile() == '':
                        self.state.loadInstrument(a)
        
        self.view.showMainWindow()
    
    def initDynamicView(self):
        # load installed mcstas instruments:
        # construct args = [site, instr_fullpath[], instr_path_lst[]]
        args = []
        files_instr, files_comp = McGuiUtils.getInstrumentAndComponentFiles(mccode_config.configuration["MCCODE_LIB_DIR"])
        
        # temporary list consisting of instrument files with site names: 
        files_instr_and_site = []
        for f in files_instr:
            files_instr_and_site.append([f, McGuiUtils.getInstrumentSite(f)])
        
        # order instrument files by site:
        sites = {s for s in map(lambda f: f[1], files_instr_and_site)}
        for s in sites:
            # extract instruments file paths of this site
            instr_path_lst = map(lambda f: f[0], filter(lambda f: f[1] in [s], files_instr_and_site))
            # sort instrument of this site by file name
            instr_path_lst.sort(key=lambda instrpath: os.path.splitext(os.path.basename(instrpath))[0])
            # extract file names
            instr_name_lst = map(lambda instrpath: os.path.splitext(os.path.basename(instrpath))[0], instr_path_lst)
            arg = []
            arg.append(s)
            arg.append(instr_name_lst)
            arg.append(instr_path_lst)
            args.append(arg)
        
        # sort sites 
        args.sort(key=lambda arg: arg[0])    
        
        # hand on for menu generation
        self.view.initMainWindowDynamicElements(args, self.handleNewFromTemplate)
        
        # load installed mcstas components:
        # args - [category, comp_names[], comp_parsers[]]
        args = []
        categories = {0 : 'Source', 1 : 'Optics', 2 : 'Sample', 3 : 'Monitor', 4 : 'Misc', 5 : 'Contrib', 6 : 'Obsolete'}
        dirnames = {0 : 'sources', 1 : 'optics', 2 : 'samples', 3 : 'monitors', 4 : 'misc', 5 : 'contrib', 6 : 'obsolete'}
        i = 0
        while i < 7:
            arg = [] # arg - category, comp_names[], comp_parsers[]
            compnames = []
            parsers = []
            
            for f in files_comp:
                if re.search(dirnames[i], f):
                    compnames.append(os.path.splitext(os.path.basename(f))[0]) # get filename without extension - this is the component name
                    parsers.append(McComponentParser(f)) # append a parser, for ease of parsing on-the-fly
            
            arg.append(categories[i])
            arg.append(compnames)
            arg.append(parsers)
            args.append(arg)
            
            i += 1
        
        # sort components in each category (using Python default string sort on filename)
        for arg in args:
            arg[1].sort()
            arg[2].sort(key=lambda parser: os.path.splitext(os.path.basename(parser.file))[0])
        
        # hand on for menu generation
        self.view.initCodeEditorComponentMenu(args)

    ''' UI callbacks
    '''
    def handleRunOrInterruptSim(self):
        if self.state.isSimRunning():
            self.state.interrupt()
        else:
            # auto-save instrument file 
            self.view.ew.save()
            
            self.emitter.status("Getting instrument params...")
            try:
                instr_params = self.state.getInstrParams()
            except:
                self.emitter.status("Instrument compile error")
                raise
                
            fixed_params, new_instr_params = self.view.showStartSimDialog(instr_params)
            
            self.emitter.status("")
            
            if fixed_params != None:
                self.state.run(fixed_params, new_instr_params)
        
    def handleConfiguration(self):
        self.view.showConfigDialog()
        
    def handleChangeWorkDir(self):
        workDir = self.view.showChangeWorkDirDlg(self.state.getWorkDir())
        if workDir:
            self.state.setWorkDir(workDir)
    
    def handleExit(self):
        if self.view.closeCodeEditorWindow():
            sys.exit()
    
    def handlePlotResults(self):
        self.emitter.status('')
        resultdir = self.state.getDataDir()
        cmd = mccode_config.configuration["MCPLOT"] + ' ' + resultdir
        subprocess.Popen(cmd, 
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT,
                         shell=True)
        self.emitter.message(cmd)
        self.emitter.message('')
        
    def handleHelpWeb(self):
        # open the mcstas homepage
        mcurl = 'http://www.mcstas.org'
        webbrowser.open_new_tab(mcurl)
    
    def handleHelpPdf(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ..)
        mcman = os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "doc", "manuals", "mcstas-manual.pdf")
        webbrowser.open_new_tab(mcman)
    
    def handleHelpPdfComponents(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ...)
        mcman = os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "doc", "manuals", "mcstas-components.pdf")
        webbrowser.open_new_tab(mcman)
    
    def handleHelpAbout(self):
        # get mcstas version using 'mcstas/mcxtrace -v'
        process = subprocess.Popen(mccode_config.configuration["MCCODE"] +' -v', 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        # synchronous
        (stdoutdata, stderrdata) = process.communicate()
        
        self.view.showAboutBox(stdoutdata)
        
    def handleEditInstrument(self):
        instr = self.state.getInstrumentFile()
        self.view.showCodeEditorWindow(instr)
        self.emitter.status("Editing instrument: " + os.path.basename(str(instr)))
        
    def handleCloseInstrument(self):
        if self.view.closeCodeEditorWindow():
            self.state.unloadInstrument()
            self.emitter.message("Instrument closed")
            self.emitter.status("Instrument closed")
    
    def handleSaveInstrument(self, text):
        result = self.state.saveInstrumentIfFileExists(text)
        if result:
            self.view.ew.assumeDataSaved()
            self.emitter.status("Instrument saved: " + os.path.basename(self.state.getInstrumentFile()))
            
    def handleSaveAs(self):
        oldinstr = self.state.getInstrumentFile()
        if oldinstr != '':
            newinstr = self.view.showSaveAsDialog(oldinstr)
        
        if newinstr != '':
            self.state.unloadInstrument()
            text = McGuiUtils.getFileContents(oldinstr)
            created_instr = McGuiUtils.saveInstrumentFile(newinstr, text)
            if created_instr != '':
                self.state.loadInstrument(created_instr)
                self.emitter.status("Instrument saved as: " + newinstr)
    
    def handleNewInstrument(self):
        new_instr_req = self.view.showNewInstrDialog(self.state.getWorkDir())
        if new_instr_req != '':
            template_text_header = open(os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "examples", "template_header_simple.instr")).read()
            template_text_body = open(os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "examples", "template_body_simple.instr")).read()
            new_instr = McGuiUtils.saveInstrumentFile(new_instr_req, template_text_header + template_text_body)
            if new_instr != '':
                if self.view.closeCodeEditorWindow():
                    self.state.unloadInstrument()
                    self.state.loadInstrument(new_instr)
                    self.view.showCodeEditorWindow(new_instr)
                    self.emitter.status("Editing new instrument: " + os.path.basename(str(new_instr)))
        
    def handleNewFromTemplate(self, instr_templ=''):
        new_instr_req = self.view.showNewInstrFromTemplateDialog(os.path.join(self.state.getWorkDir(), os.path.basename(str(instr_templ))))
        if new_instr_req != '':
            if self.view.closeCodeEditorWindow():
                text = McGuiUtils.getFileContents(instr_templ)
                new_instr = McGuiUtils.saveInstrumentFile(new_instr_req, text)
                self.state.loadInstrument(new_instr)
                self.emitter.status("Instrument created: " + os.path.basename(str(new_instr)))
        
    def handleOpenInstrument(self):
        instr = self.view.showOpenInstrumentDlg(self.state.getWorkDir())
        if instr:
            if self.view.closeCodeEditorWindow():
                self.state.unloadInstrument()
                self.state.loadInstrument(instr)
                self.emitter.message("Instrument opened: " + os.path.basename(str(instr)))
                self.emitter.status("Instrument: " + os.path.basename(str(instr)))
        
    def handleMcdoc(self):
        subprocess.Popen('mcdoc', shell=True)
    
    ''' Connect UI and state callbacks 
    '''
    def connectCallbacks(self):        
        # connect UI widget signals to our handlers/logics
        # NOTICE: This explicit widget access is an exception - all widget access is otherwise handled by the view classes
        
        mwui = self.view.mw.ui
        mwui.actionQuit.triggered.connect(self.handleExit)
        mwui.actionOpen_instrument.triggered.connect(self.handleOpenInstrument)
        mwui.actionClose_Instrument.triggered.connect(self.handleCloseInstrument)
        mwui.actionEdit_Instrument.triggered.connect(self.handleEditInstrument)
        mwui.actionSave_As.triggered.connect(self.handleSaveAs)
        mwui.actionNew_Instrument.triggered.connect(self.handleNewInstrument)
        mwui.actionConfiguration.triggered.connect(self.handleConfiguration)
        
        mwui.btnRun.clicked.connect(self.handleRunOrInterruptSim)
        mwui.btnPlot.clicked.connect(self.handlePlotResults)
        mwui.btnEdit.clicked.connect(self.handleEditInstrument)
        mwui.btnOpenInstrument.clicked.connect(self.handleOpenInstrument)
        
        mwui.actionCompile_Instrument.triggered.connect(self.state.compile)
        mwui.actionCompile_Instrument_MPI.triggered.connect(lambda: self.state.compile(mpi=True))
        mwui.actionRun_Simulation.triggered.connect(self.handleRunOrInterruptSim)
        mwui.actionPlot.triggered.connect(self.handlePlotResults)
        
        mwui.actionMcdoc.triggered.connect(self.handleMcdoc)
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        mwui.actionMcstas_Component_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        ew = self.view.ew
        ew.saveRequest.connect(self.handleSaveInstrument)
        
        st = self.state
        st.simStateUpdated.connect(self.view.updateSimState)
        st.instrumentUpdated.connect(self.view.updateInstrument)
        
        emitter = self.emitter
        emitter.statusUpdate.connect(self.view.updateStatus)
        emitter.logMessageUpdate.connect(self.view.updateLog)

''' Last resort exception handler
'''
def handleExceptionMsg(msg):
    print(msg)

''' Program execution
'''
def main():
    try:
        McGuiUtils.loadUserConfig(mccode_config.configuration["MCCODE"],mccode_config.configuration["MCCODE_VERSION"])
        
        mcguiApp = QtGui.QApplication(sys.argv)
        mcguiApp.ctr = McGuiAppController()
        
        sys.exit(mcguiApp.exec_())

    except Exception, e: 
        print(e.message)
        raise
        
        
if __name__ == '__main__':
    main()

