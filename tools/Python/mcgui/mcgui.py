#!/usr/bin/env python3
'''
mcgui program 

Initial version: spring of 2015.
Release version: fall 2016.
'''
import sys
import os
import webbrowser
import subprocess
import time
import re
from PyQt5 import QtCore, QtWidgets
import PyQt5
from viewclasses import McView
from datetime import datetime

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from mccodelib import mccode_config, utils
from mccodelib.utils import ComponentParser, get_instr_site, get_instr_comp_files, save_instrfile, get_file_contents

''' Message emitter
Status and message log and signalling.
'''
class McMessageEmitter(QtCore.QObject):
    statusUpdate = QtCore.pyqtSignal(str)
    __statusLog = []
    
    logMessageUpdate = QtCore.pyqtSignal(str, bool, bool)
    __msgLog = []
    
    def status(self, status):
        ''' status / current general state info
        '''
        if status == None:
            return
        
        self.statusUpdate.emit(status)
        self.__statusLog.append(status)
        
    
    def message(self, msg, err_msg=False, gui=False):
        ''' message log messages (simulation progress etc.)
        '''
        if msg == None:
            return
        
        self.logMessageUpdate.emit(msg, err_msg, gui)
        self.__msgLog.append(msg)
        # NOTE: calling processEvents too often can lead to some sort of stack overflow, but side effects are to be investigated
        #QtWidgets.QApplication.processEvents()


''' Asynchronous process execution QThread
'''
class McRunQThread(QtCore.QThread):
    thread_exception = QtCore.pyqtSignal(str)
    error = QtCore.pyqtSignal(str)
    message = QtCore.pyqtSignal(str)
    cmd = ''
    cwd = ''
    process_returncode = None
    
    def run(self, *args, **kwargs):
        try:
            # check cmd is set
            if self.cmd == '':
                raise Exception("McRunQThread: Set cmd before running 'start'")

            # open a subprocess with shell=True, otherwise stdout will be buffered and thus 
            # not readable live
            process = subprocess.Popen(self.cmd, 
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT,
                                       stdin=subprocess.PIPE,
                                       shell=True,
                                       universal_newlines=True,
                                       cwd=self.cwd)

            for stdoutdata in process.stdout:
                self.message.emit(stdoutdata.rstrip('\n'))

            self.process_returncode = process.returncode
        except Exception as e:
            self.thread_exception.emit(str(e))
            

''' State
Holds unique state values and mediates some low-level actions.
'''
class McGuiState(QtCore.QObject):
    __instrFile = ''
    __emitter = None
    
    # <instrument>, <work dir>
    instrumentUpdated = QtCore.pyqtSignal(list, str)
    # [<canRun>, <canPlot>] each can be str 'True' or 'False'
    simStateUpdated = QtCore.pyqtSignal(list)
    
    __cFile = None
    __binaryFile = None
    __dataDir = ""
    
    def __init__(self, emitter):
        super(McGuiState, self).__init__()
        self.setWorkDir(os.getcwd())
        self.__emitter = emitter
        
    def __fireInstrUpdate(self):
        self.instrumentUpdated.emit([self.getInstrumentFile(), self.getWorkDir()], self.getInstrumentFile())
        
    def __fireSimStateUpdate(self):
        self.simStateUpdated.emit([str(self.canRun()), str(self.canPlot()), str(self.isSimRunning())])
    
    def init(self):
        ''' must be called after construction to emit events used for sync'ing to initial state '''
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
    
    def getInstrumentFile(self):
        return self.__instrFile
    
    def loadInstrument(self, instr_file):
        def get_bin_and_c_filenames(instr):
            # assume instr exists
            base = os.path.splitext(instr)[0]
            
            c_fn = base + '.c'
            c_fn_exists = os.path.exists(c_fn)
            
            bin_fn = base + '.out'
            bin_fn_exists = os.path.exists(bin_fn)
            
            return c_fn if c_fn_exists else None, bin_fn if bin_fn_exists else None
        
        # makes sure this is not a qstring
        instr_file = str(instr_file)
        # file must exist:
        if os.path.exists(instr_file):
            # handle .instr files loaded without full path
            if os.path.dirname(instr_file) != '':
                realdir = os.path.dirname(os.path.abspath(instr_file))
                self.setWorkDir(realdir)
            
            instr_file = os.path.join(self.getWorkDir(), os.path.basename(instr_file))
            self.__instrFile = instr_file
            self.__cFile, self.__binaryFile = get_bin_and_c_filenames(self.__instrFile)
            self.__fireInstrUpdate()
            self.__emitter.status("Instrument: " + os.path.basename(self.__instrFile))
            self.__fireSimStateUpdate()
            
        else:
            # TODO: throw exception
            self.__emitter.status('Could not load file: ' + instr_file)
            #raise Exception("McGuiState.loadInstrument: Error. File: " + instr_file)
        
    def unloadInstrument(self):
        self.__instrFile = ''
        self.__cFile = None
        self.__binaryFile = None
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
        return True
    
    def saveInstrumentIfFileExists(self, text):
        instr = self.getInstrumentFile()
        if not os.path.exists(instr):
            return False
        save_instrfile(instr, text)
        return True
    
    def getWorkDir(self):
        return os.getcwd()

    def getDataDir(self):
        return self.__dataDir
    
    def checkInstrFileCandidate(self, instr):
        if ' ' in instr:
            raise Exception("checkInstrFileCandidate: \s in instr")

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
    
    __thread_exc_signal = QtCore.pyqtSignal(str)
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

        # that wait cursor
        QtWidgets.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        def compile_complete():
            self.__emitter.message('compile thread done\n')
            QtWidgets.QApplication.restoreOverrideCursor()

        # compile simulation in a background thread
        self.compilethread = QtCore.QThread()
        self.compilethread.run = lambda: self.compileAsync(mpi, self.__thread_exc_signal)
        self.compilethread.finished.connect(compile_complete)
        self.compilethread.start()
        
    def compileAsync(self, mpi, thread_exc_signal):
        try:
            # generate mcstas .c file from instrument
            nf = os.path.basename(self.__instrFile)
            # paths and filenames
            spl = os.path.splitext(os.path.basename(str(nf)))
            basenoext = spl[0]
            cf = basenoext + '.c'
            bf = basenoext + '.' + mccode_config.platform["EXESUFFIX"]

            # Honour CFLAGS etc. in terminal environment:
            os.environ[mccode_config.configuration["MCCODE"].upper() + "_OVERRIDE"] = mccode_config.configuration["MCCODE_LIB_DIR"]
            os.environ[mccode_config.configuration["MCCODE"].upper() + "_CFLAGS_OVERRIDE"] = mccode_config.compilation["CFLAGS"]
            os.environ[mccode_config.configuration["MCCODE"].upper() + "_CC_OVERRIDE"] = mccode_config.compilation["CC"]
            os.environ[mccode_config.configuration["MCCODE"].upper() + "_MPICC_OVERRIDE"] = mccode_config.compilation["MPICC"]

            if mpi:
                cmd = mccode_config.configuration["MCRUN"] + ' -c --mpi=1 ' + nf + ' -n0 '
            else:
                cmd = mccode_config.configuration["MCRUN"] + ' -c ' + nf + ' -n0 '
            process = subprocess.Popen(cmd, 
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       shell=True,
                                       universal_newlines=True,
                                       cwd=os.path.dirname(self.__instrFile))
            self.__emitter.status('Compiling instrument via ' + mccode_config.configuration["MCRUN"])
            self.__emitter.message('Setting environment:', gui=True)
            self.__emitter.message(mccode_config.configuration["MCCODE"].upper() + "_OVERRIDE" + "=\n " + mccode_config.configuration["MCCODE_LIB_DIR"] + "\n", gui=True)
            self.__emitter.message(mccode_config.configuration["MCCODE"].upper() + "_CFLAGS_OVERRIDE" + "=\n " + mccode_config.compilation["CFLAGS"] + "\n", gui=True)
            self.__emitter.message(mccode_config.configuration["MCCODE"].upper() + "_CC_OVERRIDE" + "=\n " + mccode_config.compilation["CC"] + "\n", gui=True)
            self.__emitter.message(mccode_config.configuration["MCCODE"].upper() + "_MPICC_OVERRIDE" + "=\n " + mccode_config.compilation["MPICC"] + "\n", gui=True)
            self.__emitter.message(cmd, gui=True)
            
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
        # interrupts any running simulation
        if self.__runthread:
            if self.__runthread.isRunning():
                self.__runthread.terminate()
        else:
            raise Exception('State.interrupt: no __runthread')
    
    def run(self, fixed_params, params, inspect=None):
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
        
        # mpi-related
        clustering = fixed_params[4]
        mpicount = fixed_params[5]
        if mpicount == '':
            mpicount = '2'
        if clustering == 1:
            mcrunparms = ' --mpi=' + mpicount + ' '
        elif clustering == 2:
            mcrunparms = ' -c --mpi=' + mpicount + ' '
        
        # sim/trace and output directory
        simtrace = fixed_params[0]
        if simtrace == 0:
            output_dir = str(fixed_params[7])
            if output_dir == '':
                DATE_FORMAT_PATH = "%Y%m%d_%H%M%S"
                output_dir = "%s_%s" % \
                              (os.path.splitext(self.__instrFile)[0],
                               datetime.strftime(datetime.now(), DATE_FORMAT_PATH))
                
            runstr = mccode_config.configuration["MCRUN"] + mcrunparms + os.path.basename(self.__instrFile) + ' -d ' + output_dir
            self.__dataDir = output_dir
        else:
            if inspect:
                runstr = mccode_config.configuration["MCDISPLAY"] + ' ' + os.path.basename(self.__instrFile) + ' --no-output-files' + ' --inspect=' + inspect
            else:
                runstr = mccode_config.configuration["MCDISPLAY"] + ' ' + os.path.basename(self.__instrFile) + ' --no-output-files'
        
        # neutron count
        ncount = fixed_params[1]
        if int(float(ncount)) > 0:
            runstr = runstr + ' -n ' + str(ncount)
        
        # steps count
        nsteps = fixed_params[2]
        if nsteps != '':
            if int(nsteps) > 1:
                print('Nsteps is ' + nsteps)
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
        
        # autoplot
        autoplot = fixed_params[8]
        if autoplot and simtrace == 0:
            runstr = runstr + ' --autoplot' +  ' --autoplotter=' + mccode_config.configuration["MCPLOT"]
        
        # parse instrument params
        for p in params:
            runstr = runstr + ' ' + p[0] + '=' + p[1]
        
        print('Running: ' + runstr)
        
        # Add & for backgrounding on Unix systems
        if simtrace == 1 and not os.name == 'nt':
            runstr = runstr + ' &'

        # Add prefix 'start' to background on Windows systems
        if os.name == 'nt':
            runstr = 'start ' + runstr
            
        # Ensure assembled runstr is a string, not a QString 
        runstr = str(runstr)
		
        if not os.name=='nt':
        # run simulation in a background thread
            self.__runthread = McRunQThread()
            self.__runthread.cmd = runstr
            self.__runthread.cwd = os.path.dirname(self.__instrFile)
            self.__runthread.finished.connect(lambda: self.__runFinished(self.__runthread.process_returncode))
            self.__runthread.thread_exception.connect(handleExceptionMsg)
            self.__runthread.error.connect(lambda msg: self.__emitter.message(msg, err_msg=True))
            self.__runthread.message.connect(lambda msg: self.__emitter.message(msg))
            self.__runthread.start()
            self.__emitter.message(runstr, gui=True)
            self.__emitter.status('Running simulation ...')
            self.__fireSimStateUpdate()
        else:
            self.__emitter.message(runstr, gui=True)
            self.__emitter.status('Started simulation/trace in background shell...')
            subprocess.Popen(runstr, shell=True)

    def __runFinished(self, process_returncode):
        self.__fireSimStateUpdate()
        if process_returncode == 0:
            self.__emitter.message('simulation done', gui=True)
        self.__emitter.message('')
        self.__emitter.status('')


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
        
        # Print MCCODE version info in main window
        cmd = mccode_config.configuration["MCCODE"] + ' -v '
        process = subprocess.Popen(cmd, 
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE,
                                   shell=True,
                                   universal_newlines=True)

        (stdoutdata, stderrdata) = process.communicate()
        self.emitter.message(stdoutdata.rstrip('\n'))
        self.emitter.message(stderrdata.rstrip('\n'))
        # Print MCCODE revision data if these exist
        comprev = os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "revision")
        if os.path.exists(comprev):
            self.emitter.message(open(comprev).read(), gui=True)
        
        
        # load instrument file from command line pars, skipping scriptfile
        for a in sys.argv:
            if os.path.isfile(a):
                if not os.path.splitext(a)[1] == '.py':
                    if self.state.getInstrumentFile() == '':
                        self.state.loadInstrument(a)
                        
        # Shouldn't really be necessary, but otherwise App menu is inactive on macOS
        # (was initially put in message/status update mechanism, but that caused other side-effects, see
        #  https://github.com/McStasMcXtrace/McCode/issues/570 )
        QtWidgets.QApplication.processEvents()
        self.view.showMainWindow()
        
    def initDynamicView(self):
        # load installed mcstas instruments:
        # construct args = [site, instr_fullpath[], instr_path_lst[]]
        args = []
        files_instr, files_comp = get_instr_comp_files(mccode_config.configuration["MCCODE_LIB_DIR"])
        
        # temporary list consisting of instrument files with site names: 
        files_instr_and_site = []
        for f in files_instr:
            files_instr_and_site.append([f, get_instr_site(f)])
        
        # order instrument files by site:
        sites = {s for s in list(map(lambda f: f[1], files_instr_and_site))}
        for s in sites:
            # extract instruments file paths of this site
            instr_path_lst = list(map(lambda f: f[0], filter(lambda f: f[1] in [s], files_instr_and_site)))
            # sort instrument of this site by file name
            instr_path_lst.sort(key=lambda instrpath: os.path.splitext(os.path.basename(instrpath))[0])
            # extract file names
            instr_name_lst = list(map(lambda instrpath: os.path.splitext(os.path.basename(instrpath))[0], instr_path_lst))
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
        categories = {0 : 'Source', 1 : 'Optics', 2 : 'Sample', 3 : 'Monitor', 4 : 'Misc', 5 : 'Contrib', 6: 'Union', 7 : 'Obsolete'}
        dirnames = {0 : 'sources', 1 : 'optics', 2 : 'samples', 3 : 'monitors', 4 : 'misc', 5 : 'contrib', 6: 'contrib/union', 7 : 'obsolete'}
        i = 0
        while i < 8:
            arg = [] # arg - category, comp_names[], comp_parsers[]
            compnames = []
            parsers = []
            
            for f in files_comp:
                if i==6:
                    if re.search(dirnames[i], os.path.dirname(f)):
                        compnames.append(os.path.splitext(os.path.basename(f))[0]) # get filename without extension - this is the component name
                        parsers.append(ComponentParser(f)) # append a parser, for ease of parsing on-the-fly
                else:
                    if re.search(dirnames[i], os.path.basename(os.path.dirname(f))):
                        compnames.append(os.path.splitext(os.path.basename(f))[0]) # get filename without extension - this is the component name
                        parsers.append(ComponentParser(f)) # append a parser, for ease of parsing on-the-fly
            
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
            # ensure auto-save
            self.view.ew.save()

            self.emitter.status("Getting instrument params...")
            QtWidgets.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
            self.view.disableRunBtn()
            try:
                class ThreadInfoHandler():
                    def __init__(self, forwardmsg=None):
                        self.stdout_lst = []
                        self.finished = False
                        self.forwardmsg = forwardmsg
                    def stdout(self, msg):
                        self.stdout_lst.append(msg)
                        if self.forwardmsg:
                            self.forwardmsg(msg)
                    def finish(self):
                        self.finished = True
                def msg(msg, em=self.emitter):
                    em.message(msg)
                handler = ThreadInfoHandler(lambda msg, em=self.emitter: em.message(msg))

                somethread = McRunQThread()
                somethread.cmd = mccode_config.configuration["MCRUN"] + ' ' + os.path.basename(self.state.getInstrumentFile()) + " --info"
                somethread.cwd = os.path.dirname(self.state.getInstrumentFile())
                somethread.finished.connect(handler.finish)
                somethread.thread_exception.connect(handleExceptionMsg)
                somethread.error.connect(lambda msg: self.emitter.message(msg, err_msg=True))
                somethread.message.connect(handler.stdout)
                somethread.start()

                while not handler.finished:
                    time.sleep(0.2)
                    QtWidgets.QApplication.processEvents()

                params = []
                for l in handler.stdout_lst:
                    if 'Param:' in l:
                        s = l.split()
                        s[0]=""
                        s = ' '.join(s)
                        s = s.split('=')
                        params.append(s)

                instr_params = params

            except:
                self.emitter.status("Instrument compile error")
                raise
            finally:
                QtWidgets.QApplication.restoreOverrideCursor()
                self.view.enableRunBtn()

            def get_compnames(text):
                ''' return a list of compnames from an instrument definition code text '''
                comps = []
                pat = 'COMPONENT[ ]+([\w0-9]+)[ ]*='
                for l in text.splitlines():
                    m = re.search(pat, l)
                    if m:
                        comps.append(m.group(1))
                return comps

            comps = get_compnames(text=open(self.state.getInstrumentFile(), 'rb').read().decode())
            _a, mcplots, mcdisplays = mccode_config.get_options()
            fixed_params, new_instr_params, inspect, mcdisplay, autoplotter = self.view.showStartSimDialog(
                instr_params, comps, mcdisplays, mcplots)

            if mcdisplay != None:
                mccode_config.configuration["MCDISPLAY"] = mcdisplay
            if autoplotter != None:
                mccode_config.configuration["MCPLOT"] = autoplotter

            self.emitter.status("")

            if fixed_params != None:
                self.state.run(fixed_params, new_instr_params, inspect)

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
        cwd = os.path.dirname(self.state.getInstrumentFile())

        self._runthread = McRunQThread()
        self._runthread.cmd = cmd
        self._runthread.cwd = cwd
        self._runthread.finished.connect(lambda: None)
        self._runthread.thread_exception.connect(handleExceptionMsg)
        self._runthread.error.connect(lambda msg: self.emitter.message(msg, err_msg=True))
        self._runthread.message.connect(lambda msg: self.emitter.message(msg))
        self._runthread.start()
        
        self.emitter.message(cmd, gui=True)
        self.emitter.status('Running plotter ...')

    def handlePlotOtherResults(self):
        self.emitter.status('')
        resultdir = self.view.showOpenPlotDirDlg(os.getcwd())
        if resultdir != "":
            cmd = mccode_config.configuration["MCPLOT"] + ' ' + resultdir
            cwd = os.path.dirname(os.path.dirname(resultdir))
            self._runthread = McRunQThread()
            self._runthread.cmd = cmd
            self._runthread.cwd = cwd
            self._runthread.finished.connect(lambda: None)
            self._runthread.terminated.connect(lambda: None)
            self._runthread.thread_exception.connect(handleExceptionMsg)
            self._runthread.error.connect(lambda msg: self.emitter.message(msg, err_msg=True))
            self._runthread.message.connect(lambda msg: self.emitter.message(msg))
            self._runthread.start()
        
            self.emitter.message(cmd, gui=True)
            self.emitter.status('Running plotter ...')        
    
    def handleMcDisplayWeb(self):
        self.emitter.status('Running mcdisplay-webgl...')
        try:
            cmd = 'mcdisplay-webgl --default --no-output-files -n100 ' + os.path.basename(self.state.getInstrumentFile()) + '&'
            self.emitter.message(cmd, gui=True)
            self.emitter.message('', gui=True)
            
            def messg(s): self.emitter.message(s)
            def messg_err(s): self.emitter.message(s, err_msg=True)
            utils.run_subtool_to_completion(cmd, stdout_cb=messg, stderr_cb=messg_err)
        finally:
            self.emitter.status('')
    
    def handleMcDisplay2D(self):
        self.emitter.status('Running mcdisplay-webgl...')
        try:
            cmd = 'mcdisplay-pyqtgraph --default --no-output-files -n100 ' + os.path.basename(self.state.getInstrumentFile()) + '&'
            self.emitter.message(cmd, gui=True)
            self.emitter.message('', gui=True)
            
            def messg(s): self.emitter.message(s)
            def messg_err(s): self.emitter.message(s, err_msg=True)
            utils.run_subtool_to_completion(cmd, stdout_cb=messg, stderr_cb=messg_err)
        finally:
            self.emitter.status('')
    
    def handleHelpWeb(self):
        # open the mcstas homepage
        mcurl = 'http://www.'+mccode_config.configuration["MCCODE"]+'.org'
        webbrowser.open_new_tab(mcurl)
    
    def handleHelpPdf(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ..)
        mcman = os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "doc", "manuals", mccode_config.configuration["MCCODE"]+"-manual.pdf")
        webbrowser.open_new_tab(mcman)
    
    def handleHelpPdfComponents(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ...)
        mcman = os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "doc", "manuals", mccode_config.configuration["MCCODE"]+"-components.pdf")
        webbrowser.open_new_tab(mcman)
    
    def handleHelpAbout(self):
        # get mcstas version using 'mcstas/mcxtrace -v'
        process = subprocess.Popen(mccode_config.configuration["MCCODE"] +' -v', 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.STDOUT,
                                   shell=True,
                                   universal_newlines=True)
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
            self.emitter.message("Instrument closed", gui=True)
            self.emitter.status("Instrument closed")
    
    def handleSaveInstrument(self, text):
        result = self.state.saveInstrumentIfFileExists(text)
        if result:
            self.view.ew.assumeDataSaved()
            self.emitter.status("Instrument saved: " + os.path.basename(self.state.getInstrumentFile()))
    
    def displayNotSavedWhitespaceError(self, can_throw_func, raise_err=False):
        try:
            return can_throw_func()
        except Exception as e:
            self.emitter.status("Instrument not saved")
            self.view.showErrorDialogue("Error: Instrument not saved", "Instrument files or paths should not contain white-spaces.")
            if raise_err:
                raise e
            return False
    
    def handleSaveAs(self):
        oldinstr = self.state.getInstrumentFile()
        if oldinstr != '':
            newinstr = self.view.showSaveAsDialog(oldinstr)
            if self.displayNotSavedWhitespaceError(lambda: self.state.checkInstrFileCandidate(newinstr))==False:
                return
        
        if newinstr != '':
            self.state.unloadInstrument()
            text = get_file_contents(oldinstr)
            created_instr = save_instrfile(newinstr, text)
            if created_instr != '':
                self.state.loadInstrument(created_instr)
                self.emitter.status("Instrument saved as: " + newinstr)
    
    def handleNewInstrument(self):
        new_instr_req = self.view.showNewInstrDialog(self.state.getWorkDir())
        if self.displayNotSavedWhitespaceError(lambda: self.state.checkInstrFileCandidate(new_instr_req))==False:
            return
        
        if new_instr_req != '':
            template_text = open(os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "examples", "template_simple.instr")).read()
            new_instr = save_instrfile(new_instr_req, template_text)
            if new_instr != '':
                if self.view.closeCodeEditorWindow():
                    self.state.unloadInstrument()
                    self.state.loadInstrument(new_instr)
                    self.view.showCodeEditorWindow(new_instr)
                    self.emitter.status("Editing new instrument: " + os.path.basename(str(new_instr)))
    
    def handleNewFromTemplate(self, instr_templ=''):
        new_instr_req = self.view.showNewInstrFromTemplateDialog(os.path.join(self.state.getWorkDir(), os.path.basename(str(instr_templ))))
        if self.displayNotSavedWhitespaceError(lambda: self.state.checkInstrFileCandidate(new_instr_req))==False:
            return
        
        if new_instr_req != '':
            if self.view.closeCodeEditorWindow():
                text = get_file_contents(instr_templ)
                self.state.unloadInstrument()
                new_instr = save_instrfile(new_instr_req, text)
                self.state.loadInstrument(new_instr)
                self.emitter.status("Instrument created: " + os.path.basename(str(new_instr)))
    
    def handleOpenInstrument(self):
        instr = self.view.showOpenInstrumentDlg(self.state.getWorkDir())
        if not instr:
            return
        if not os.path.isfile(instr):
            self.emitter.status("Please select a file rather than a folder. Folder load not supported.")
            return
        if self.displayNotSavedWhitespaceError(lambda: self.state.checkInstrFileCandidate(instr))==False:
            return
        
        if instr:
            if self.view.closeCodeEditorWindow():
                self.state.unloadInstrument()
                self.state.loadInstrument(instr)
                self.emitter.message("Instrument opened: " + os.path.basename(str(instr)), gui=True)
                self.emitter.status("Instrument: " + os.path.basename(str(instr)))
    
    def handleMcdoc(self):
        cmd='%sdoc' % mccode_config.get_mccode_prefix()
        if sys.platform == "win32":
            cmd='start ' + cmd + '.bat'
        subprocess.Popen(cmd, shell=True)

    def handleMcdocCurrentInstr(self):
        cmd='%sdoc' % mccode_config.get_mccode_prefix()
        if sys.platform == "win32":
            cmd ='start ' + cmd + '.bat'
        cmd = cmd + ' %s' % self.state.getInstrumentFile()
        subprocess.Popen(cmd, shell=True)

    def handleEnvironment(self):
        terminal = mccode_config.configuration["TERMINAL"]
        if not sys.platform == 'win32':
            scriptfile = mccode_config.configuration["MCCODE_LIB_DIR"] + '/environment'
        else:
            scriptfile = 'start ' + mccode_config.configuration["MCCODE_LIB_DIR"] + '\\..\\bin\\mccodego.bat'

        subprocess.Popen(terminal + ' ' + scriptfile, shell=True)
        
    def handleDefault(self):
        reply = QtWidgets.QMessageBox.question(self.view.mw,
                                           'Define system default?',
                                           'Do you want to make the current ' +  mccode_config.configuration["MCCODE"] + ' the system default?'),
 
        if reply == QtWidgets.QMessageBox.Yes:
             subprocess.Popen('postinst set_mccode_default', shell=True)
             

    def handleDefaultMcguiPy(self):
        reply = QtWidgets.QMessageBox.question(self.view.mw,
                                           'Make Python gui App default?',
                                           'Do you want to use Python ' +  mccode_config.configuration["MCCODE"] + ' gui in the macOS App?')
        if reply == QtWidgets.QMessageBox.Yes:
            subprocess.Popen('postinst osx_app_default py', shell=True)

    def handleDefaultMcguiPl(self):
        reply = QtWidgets.QMessageBox.question(self.view.mw,
                                           'Make Python gui App default?',
                                           'Do you want to use Perl ' +  mccode_config.configuration["MCCODE"] + ' gui in the macOS App?')
        if reply == QtWidgets.QMessageBox.Yes:
            subprocess.Popen('postinst osx_app_default pl', shell=True)

        
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
        self.view.mw.add_conf_menu('Open terminal env.').triggered.connect(self.handleEnvironment)
        # On macOS
        # 1) add a copy of the configuration menu to File
        # 2) add menu points for changing what the bundle opens
        if sys.platform == 'darwin':
            self.view.mw.add_conf_menu('Configuration').triggered.connect(self.handleConfiguration)
            self.view.mw.add_conf_menu('Use Python App').triggered.connect(self.handleDefaultMcguiPy)
            self.view.mw.add_conf_menu('Use Perl App').triggered.connect(self.handleDefaultMcguiPl)            

        # If not on Windows add menu point to make current mccode the system default
        if not sys.platform == 'win32':
            self.view.mw.add_conf_menu('Set as default').triggered.connect(self.handleDefault)
        mwui.btnRun.clicked.connect(self.handleRunOrInterruptSim)
        mwui.btnPlot.clicked.connect(self.handlePlotResults)
        mwui.btnEdit.clicked.connect(self.handleEditInstrument)
        mwui.btnOpenInstrument.clicked.connect(self.handleOpenInstrument)

        mwui.actionCompile_Instrument.triggered.connect(self.state.compile)
        mwui.actionCompile_Instrument_MPI.triggered.connect(lambda: self.state.compile(mpi=True))
        mwui.actionRun_Simulation.triggered.connect(self.handleRunOrInterruptSim)
        mwui.actionPlot.triggered.connect(self.handlePlotResults)
        mwui.actionPlotOther.triggered.connect(self.handlePlotOtherResults)
        mwui.actionDisplay.triggered.connect(self.handleMcDisplayWeb)
        mwui.actionDisplay_2d.triggered.connect(self.handleMcDisplay2D)

        mwui.actionMcDocCurrent.triggered.connect(self.handleMcdocCurrentInstr)
        mwui.actionMcdoc.triggered.connect(self.handleMcdoc)
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        mwui.actionMcstas_Component_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        self.view.ew.ui.actionSave_As.triggered.connect(self.handleSaveAs)

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
    # ensure keyboardinterrupt ctr-c
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    try:
        mccode_config.load_user_config()
        mccode_config.check_env_vars()

        mcguiApp = PyQt5.QtWidgets.QApplication(sys.argv)
        mcguiApp.ctr = McGuiAppController()

        sys.exit(mcguiApp.exec_())

    except Exception as e: 
        print(e)
        raise


if __name__ == '__main__':
    main()

