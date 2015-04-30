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
import threading
import re
import config
import imp
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
    __statusLock = threading.Lock()
    
    logMessageUpdate = QtCore.pyqtSignal(QtCore.QString, bool, bool)
    __msgLog = []
    __msgLock = threading.Lock()
    
    def status(self, status):
        ''' status / current general state info
        '''
        if status == None:
            return
        
        self.__statusLock.acquire()
        self.statusUpdate.emit(status)
        self.__statusLog.append(status)
        self.__statusLock.release()
        QtGui.QApplication.processEvents()
    
    def message(self, msg, mcguiMsg=False, errorMsg=False):
        ''' message log messages (simulation progress etc.)
        '''
        if msg == None:
            return
        
        self.__msgLock.acquire()
        self.logMessageUpdate.emit(msg, mcguiMsg, errorMsg)
        self.__msgLog.append(msg)
        self.__msgLock.release()
        QtGui.QApplication.processEvents()
    

''' State
Holds unique state values and mediates some low-level actions.
'''
class McGuiState(QtCore.QObject):
    __instrFile = ''
    __emitter = None
    
    instrumentUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    # [<canRun>, <canPlot>] each can be str 'True' or 'False'
    simStateUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    
    __cFile = ""
    __binaryFile = ""
    __resultFile = ""
    __DataDir = ""
    
    def __init__(self, emitter):
        super(McGuiState, self).__init__()
        self.setWorkDir(os.getcwd())
        self.__emitter = emitter
        
    def __fireInstrUpdate(self):
        self.instrumentUpdated.emit([self.getInstrumentFile(), self.getWorkDir()])
        
    def __fireSimStateUpdate(self):
        self.simStateUpdated.emit([str(self.canRun()), str(self.canPlot())])
    
    def init(self):
        ''' must be called after construction to emit events used for sync'ing to initial state '''
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
    
    def getInstrumentFile(self):
        return self.__instrFile
    
    def loadInstrument(self, instrFile):
        if not os.path.exists(str(instrFile)):
            raise Exception("McGuiState.loadInstrument: Error.")
        self.setWorkDir(os.path.dirname(str(instrFile)))
        self.__instrFile = str(instrFile)
        self.__fireInstrUpdate()
        self.__fireSimStateUpdate()
    
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
        return self.__DataDir
    
    def setWorkDir(self, newdir):
        if not os.path.isdir(newdir):
            raise Exception('McGuiState.setWorkDir: Invalid work dir.')
        os.chdir(newdir)
    
    def canCompile(self):
        return self.__instrFile != ""
    
    def canRun(self):
        return self.__instrFile != ""
    
    def canPlot(self):
        return self.__instrFile != ""
    
    def compile(self):
        # compile simulation in a background thread (non safe)
        thread = threading.Thread(target=self.compileAsync)
        thread.start()
        
    def compileAsync(self):
        # generate mcstas .c file from instrument
        nf = self.__instrFile
        cmd = config.MCCODE + ' '  + nf
        process = subprocess.Popen(cmd, 
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        self.__emitter.status('Compiling instrument to c ...')
        self.__emitter.message('Compiling instrument to c ...', mcguiMsg=True)
        self.__emitter.message(cmd, mcguiMsg=True)

        # read program output while the process is active
        while process.poll() == None:
            stdoutdata = process.stdout.readline().rstrip('\n')
            self.__emitter.message(stdoutdata)
            time.sleep(0.05)
        ## flush until EOF
        for stdoutdata in process.stdout:
            self.__emitter.message(stdoutdata.rstrip('\n'))
            
        # paths and filenames
        spl = os.path.splitext(os.path.basename(str(nf)))
        basef = os.path.join(self.getWorkDir(), spl[0])
        cf = basef + '.c'
        
        # check
        if os.path.isfile(cf):
            self.__cFile = cf
            self.__emitter.message('    --> ' + self.__cFile, mcguiMsg=True)
        else:
            raise Exception('C file not found')
        
        # compile binary from mcstas .c file 
        bf = basef + '.' + config.EXESUFFIX 
        cmd = config.CC + ' -o ' + bf + ' ' + cf + ' -lm ' + config.CFLAGS
       
        process = subprocess.Popen(cmd, 
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        self.__emitter.status('Compiling instrument to binary ...')
        self.__emitter.message('Compiling instrument to binary ...', mcguiMsg=True)
        self.__emitter.message(cmd, mcguiMsg=True)

        # read program output while the process is active
        while process.poll() == None:
            stdoutdata = process.stdout.readline().rstrip('\n')
            self.__emitter.message(stdoutdata)
            time.sleep(0.05)
        ## flush until EOF
        for stdoutdata in process.stdout:
            self.__emitter.message(stdoutdata.rstrip('\n'))
                
        # check
        if os.path.isfile(bf):
            self.__binaryFile = bf
            self.__emitter.message('    --> ' + self.__binaryFile + '\n', mcguiMsg=True)
            self.__emitter.status('Instrument compiled')
        else:
            raise Exception('Binary not found.')
        
        self.__fireSimStateUpdate()
    
    def run(self, fixed_params, params):
        ''' fixed_params[]:
                simulation = 0, trace = 1
                neutron count (int)
                steps count (int)
                gravity (bool)
                random seed (int)
                no clustering = 0, MPI clustering = 1, SSH clustering = 2
            params[]:
                [<par_name>, <value>] pairs
        '''
        DATE_FORMAT_PATH = "%Y%d%m_%H%M%S"
        dir = "%s_%s" % \
                      (self.__instrFile,
                       datetime.strftime(datetime.now(), DATE_FORMAT_PATH))
        
        runstr = config.MCRUN + ' ' + self.__instrFile + ' -d ' + dir
        self.__DataDir = dir

        # parse fixed params
        simtrace = fixed_params[0]
        # TODO: support trace
        
        ncount = fixed_params[1]
        if ncount > 0:
            runstr = runstr + ' -n ' + str(ncount)
        
        nsteps = fixed_params[2]
        if nsteps > 1:
            runstr = runstr + ' -N ' + str(nsteps)
        
        if str(fixed_params[3]) == "True":
            gravity = True
            # TODO: append runstr with the gravity option
        
        random_seed = fixed_params[4]
        # TODO: support random seed
            
        clustering = fixed_params[5]
        # TODO: support clustering
        
        # parse instrument params        
        for p in params:
            runstr = runstr + ' ' + p[0] + '=' + p[1]
        
        print(runstr)
        
        # run simulation in a background thread (non safe)
        thread = threading.Thread(target=self.runAsync(runstr))
        thread.start()
        
    def runAsync(self, runstr):
        # open a subprocess with shell=True, otherwise stdout will be buffered and thus 
        # not readable live
        process = subprocess.Popen(runstr, 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.PIPE,
                                   shell=True)
        self.__emitter.message(runstr, mcguiMsg=True)
        self.__emitter.status('Running simulation ...')
        
        # read program output while the process is active
        while process.poll() == None:
            stdoutdata = process.stdout.readline().rstrip('\n')
            self.__emitter.message(stdoutdata)
            time.sleep(0.05)
        ## flush until EOF
        for stdoutdata in process.stdout:
            self.__emitter.message(stdoutdata.rstrip('\n'))

        self.__emitter.message('', mcguiMsg=True)
        self.__emitter.status('Simulation complete.')

    def getInstrParams(self):
        # get instrument params using 'mcrun [instr] --info'
        cmd = config.MCRUN + ' ' + self.__instrFile + " --info"
        process = subprocess.Popen(cmd, 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        # synchronous
        (stdoutdata, stderrdata) = process.communicate()
        
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
            if os.path.splitext(a)[1] == '.instr':
                instr_file = os.path.abspath(a)
                self.state.loadInstrument(instr_file)        
                self.emitter.status("Instrument: " + os.path.basename(str(instr_file)))
                break
        
        self.view.showMainWindow()
    
    def initDynamicView(self):
        # load installed mcstas instruments:
        # construct args = [site, instr_fullpath[], instr_path_lst[]]
        args = []
        files_instr, files_comp = McGuiUtils.getInstrumentAndComponentFiles(config.MCCODE_LIB_DIR)
        
        # temporary list consisting of instrument files with site names: 
        files_instr_and_site = []
        for f in files_instr:
            files_instr_and_site.append([f, McGuiUtils.getInstrumentSite(f)])
        
        # order instrument files by site:
        sites = {s for s in map(lambda f: f[1], files_instr_and_site)}
        for s in sites:
            instr_path_lst = map(lambda f: f[0], filter(lambda f: f[1] in [s], files_instr_and_site))
            instr_name_lst = map(lambda i: os.path.splitext(os.path.basename(i))[0], instr_path_lst)
            arg = []
            arg.append(s)
            arg.append(instr_name_lst)
            arg.append(instr_path_lst)
            args.append(arg)
            
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
                if re.search(r'/' + dirnames[i] + r'/', f):
                    compnames.append(os.path.splitext(os.path.basename(f))[0]) # get filename without extension - this is the component name
                    parsers.append(McComponentParser(f)) # append a parser, for ease of parsing on-the-fly
            
            arg.append(categories[i])
            arg.append(compnames)
            arg.append(parsers)
            args.append(arg)
            
            i += 1
        
        # hand on for menu generation
        self.view.initCodeEditorComponentMenu(args)

    ''' UI callbacks
    '''
    def handleRunSim(self):
        self.emitter.status("Getting instrument params...")
        
        instr_params = self.state.getInstrParams()
        fixed_params, new_instr_params = self.view.showStartSimDialog(instr_params)
        
        if fixed_params != None:
            self.state.run(fixed_params, new_instr_params)
        
    def handleChangeWorkDir(self):
        workDir = self.view.showChangeWorkDirDlg(self.state.getWorkDir())
        if workDir:
            self.state.setWorkDir(workDir)
    
    def handleExit(self):
        sys.exit()
    
    def handlePlotResults(self):
        self.emitter.status('')
        resultdir = self.state.getDataDir()
        cmd = config.MCPLOT + ' ' + resultdir
        subprocess.Popen(cmd, 
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT,
                         shell=True)
        self.emitter.message(callstr, mcguiMsg=True)
        self.emitter.message('', mcguiMsg=True)
        
    def handleHelpWeb(self):
        '''open the mcstas homepage'''
        mcurl = 'http://www.mcstas.org'
        webbrowser.open_new_tab(mcurl)
    
    def handleHelpPdf(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ..)
        mcman = config.MCCODE_LIB_DIR + '/doc/manuals/mcstas-manual.pdf'
        webbrowser.open_new_tab(mcman)
    
    def handleHelpPdfComponents(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ...)
        mcman = config.MCCODE_LIB_DIR + '/doc/manuals/mcstas-components.pdf'
        webbrowser.open_new_tab(mcman)
    
    def handleHelpAbout(self):
        print("not implemented")
        
    def handleEditInstrument(self):
        instr = self.state.getInstrumentFile()
        self.view.showCodeEditorWindow(instr)
        self.emitter.status("Editing instrument: " + os.path.basename(str(instr)))
        
    def handleCloseInstrument(self):
        if self.view.closeCodeEditorWindow():
            self.state.unloadInstrument()
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
            new_instr = McGuiUtils.saveInstrumentFile(new_instr_req, '')
            if new_instr != '':
                self.state.unloadInstrument()
                # TODO: insert instrument template into the new instrument
                self.state.loadInstrument(new_instr)
                self.view.showCodeEditorWindow(new_instr)
                self.emitter.status("Editing new instrument: " + os.path.basename(new_instr))
    
    def handleNewFromTemplate(self, instr_templ=''):
        new_instr_req = self.view.showNewInstrFromTemplateDialog(self.state.getWorkDir() + '/' + os.path.basename(str(instr_templ)))
        if new_instr_req != '':
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
                self.emitter.status("Instrument: " + os.path.basename(str(instr)))
            
    ''' Connect UI and state callbacks 
    '''
    def connectCallbacks(self):        
        # connect UI widget signals to our handlers/logics
        # NOTICE: This explicit widget access is exceptional - all widget access is otherwise handled by the view classes
        
        mwui = self.view.mw.ui
        mwui.actionQuit.triggered.connect(self.handleExit)
        mwui.actionOpen_instrument.triggered.connect(self.handleOpenInstrument)
        mwui.actionClose_Instrument.triggered.connect(self.handleCloseInstrument)
        mwui.actionEdit_Instrument.triggered.connect(self.handleEditInstrument)
        mwui.actionSave_As.triggered.connect(self.handleSaveAs)
        mwui.actionNew_Instrument.triggered.connect(self.handleNewInstrument)
        
        mwui.btnRun.clicked.connect(self.handleRunSim)
        mwui.btnPlot.clicked.connect(self.handlePlotResults)
        mwui.btnEdit.clicked.connect(self.handleEditInstrument)
        mwui.btnOpenInstrument.clicked.connect(self.handleOpenInstrument)
        
        mwui.actionCompile_Instrument.triggered.connect(self.state.compile)
        mwui.actionRun_Simulation.triggered.connect(self.handleRunSim)
        
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        mwui.actionMcstas_Component_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        ew = self.view.ew
        ew.saveRequest.connect(self.handleSaveInstrument)
        
        st = self.state
        st.simStateUpdated.connect(self.view.updateSimState)
        st.instrumentUpdated.connect(self.view.updateInstrumentLabel)
        
        emitter = self.emitter
        emitter.statusUpdate.connect(self.view.updateStatus)
        emitter.logMessageUpdate.connect(self.view.updateLog)
        
            
''' Program execution
'''
def main():
    userconfig=os.path.expandvars("$HOME/.mcstas/config.py")
    if os.path.isfile(userconfig):
        print "Loading user configuration from "+userconfig
        imp.load_source('config', userconfig)
    mcguiApp = QtGui.QApplication(sys.argv)
    mcguiApp.ctr = McGuiAppController()
    
    sys.exit(mcguiApp.exec_())

if __name__ == '__main__':
    main()

