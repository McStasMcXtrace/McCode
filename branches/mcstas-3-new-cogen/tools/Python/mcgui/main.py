#!/usr/bin/env python
'''mgcui 

Initial version written during Winter/spring of 2015

@author: jaga
'''
import sys
import os
import webbrowser
import subprocess
import time
import threading
from PyQt4 import QtGui
from PyQt4 import QtCore
from viewclasses import McView


''' State
Holds unique state values.
'''
class McGuiState(QtCore.QObject):    
    def initState(self):
        self.setWorkDir(os.getcwd())
        
        # load instrument file from command line pars
        for a in sys.argv:
            if os.path.splitext(a)[1] == '.instr':
                instr_file = os.path.abspath(a)
                self.loadInstument(instr_file)
                break
        
        self.fireSimStateUpdate()
    
    status = ''
    statusUpdated = QtCore.pyqtSignal(QtCore.QString)
    def setStatus(self, status):
        self.msgLock.acquire()
        self.status = status
        self.statusUpdated.emit(self.status)
        self.msgLock.release()
        
    logMessage = QtCore.pyqtSignal(QtCore.QString, bool, bool)
    msgLock = threading.Lock()
    def __logLine(self, logline, mcguiMsg=False, errorMsg=False):
        if mcguiMsg:
            logline = 'mcgui:      ' + logline
        self.msgLock.acquire()
        if logline != None:
            self.logMessage.emit(logline, mcguiMsg, errorMsg)
        self.msgLock.release()
        
    __instrFile = ''
    instrumentUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    def __fireInstrUpdate(self):
        self.instrumentUpdated.emit([self.getInstrumentFile(), self.getWorkDir()])
    
    def getInstrumentFile(self):
        return self.__instrFile
    
    def getInstrContents(self):
        instr = self.getInstrumentFile()
        if os.path.exists(str(instr)):
            f = open(instr, 'r')
            text = f.read()
            f.close()
            return text
    
    def loadInstument(self, instrFile, unload=False):
        if not unload:
            if not os.path.exists(str(instrFile)):
                return False
                #TODO: make exception error messages work
                #raise Exception('Invalid instrument file.')
                
        self.__instrFile = str(instrFile)
        self.__fireInstrUpdate() 
        if not unload:
            self.setStatus("Instrument opened: " + instrFile)
        else:
            self.setStatus("Instrument closed")
        self.fireSimStateUpdate()
        return True
    
    def saveInstrumentIfFileExists(self, text):
        instr = self.getInstrumentFile()
        if not os.path.exists(instr):
            return False
        
        # TODO: add try-finally and error handling
        f = open(instr, 'w')
        f.write(text)
        f.close()
        
        return True
    
    def createNewInstrumentFile(self, instr, text):
        if instr == '':
            return ''
        
        if str(instr).find('.') >= 0:
            if os.path.splitext(str(instr))[1] != '.instr':
                instr = instr + '.instr'
        else:
            instr = instr + '.instr'
        
        # TODO: add try-finally and error handling
        f = open(instr, 'w')
        f.write(text)
        f.close()
        
        return instr
    
    def getWorkDir(self):
        return os.getcwd()
    
    def setWorkDir(self, newdir):
        if not os.path.isdir(newdir):
            raise Exception('Invalid work dir.')
        
        olddir = os.getcwd()
        os.chdir(newdir)
        
        if newdir != olddir:
            self.__binaryFile = ''
            self.__cFile = ''
        
        self.setStatus("Work dir: " + newdir)
        self.__fireInstrUpdate()
    
    __cFile = ""
    __binaryFile = ""
    __resultFile = ""
    def canCompile(self):
        return self.__instrFile != ""
    
    def canRun(self):
        return self.__instrFile != ""
    
    def canPlot(self):
        return False
        
    # [<canRun>, <canPlot>] each can be str 'True' or 'False'
    simStateUpdated = QtCore.pyqtSignal(QtCore.QStringList)
    def fireSimStateUpdate(self):
        self.simStateUpdated.emit([str(self.canRun()), str(self.canPlot())])
    
    def compile(self):
        # compile simulation in a background thread (non safe)
        thread = threading.Thread(target=self.compileAsync)
        thread.start()
        
    def compileAsync(self):
        # create mcstas .c file from instrument
        nf = self.__instrFile
        process = subprocess.Popen(['mcstas', nf], 
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        self.__logLine('Compiling instrument to c ...', mcguiMsg=True)
        self.setStatus('Compiling instrument to c ...')
        while process.poll() <> 0:
            for l in process.stdout:
                self.__logLine(l)
            time.sleep(0.1)
        process.wait()
        
        # paths and filenames
        spl = os.path.splitext(os.path.basename(str(nf)))
        basef = os.path.join(self.getWorkDir(), spl[0])
        cf = basef + '.c'
        if os.path.isfile(cf):
            self.__cFile = cf
        else:
            raise Exception('C file not found')
        
        # compile binary from mcstas .c file 
        bf = basef + '.out'
        process = subprocess.Popen(['cc', '-O', '-o', bf, cf, '-lm'], 
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        self.__logLine('Compiling instrument to binary ...', mcguiMsg=True)
        self.setStatus('Compiling instrument to binary ...')
        while process.poll() <> 0:
            for l in process.stdout:
                self.__logLine(l)
            time.sleep(0.1)
        process.wait()
        
        # check
        if os.path.isfile(bf):
            self.__binaryFile = bf
        else:
            raise Exception('Binary not found.')
        
        self.__logLine('Instrument compiled: ' + self.__binaryFile, mcguiMsg=True)
        self.setStatus('Instrument compiled (' + self.__binaryFile + ')')
        self.fireSimStateUpdate()
    
    def run(self, fixed_params, params):
        ''' fixed_params[]:
                simulation = 0, trace = 1
                neutron count (int)
                steps count (int)
                gravity (bool)
                random seed (int)
                no clustering = 0, MPI clustering = 1, SSH clustering = 2
            params[]:
                [<name>, <value>] pairs
        '''
        runstr = 'mcrun ' + self.__instrFile
        
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
            s = p[0] + '=' + p[1]
            runstr = runstr + ' ' + s
        
        print(runstr)
        
        # run simulation in a background thread (non safe)
        thread = threading.Thread(target=self.runAsync(runstr))
        thread.start()
        
    def runAsync(self, runstr):
        process = subprocess.Popen([runstr], 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.PIPE,
                                   shell=True)
        self.__logLine('mcrun started (' + runstr + ')', mcguiMsg=True)
        self.setStatus('Running simulation ...')

        
        ## read program output
        while process.poll() == None:
            for l in process.stdout:
                self.__logLine(l.rstrip('\n'))
            for l in process.stderr:
                self.__logLine(l.rstrip('\n'), errorMsg=True)
            time.sleep(0.1)
        process.wait()
        
        self.__logLine('mcrun complete', mcguiMsg=True)
        self.setStatus('Simulation complete.')

    def getInstrParams(self):
        # get mcrun to print '--info' containing instrument parameter info
        process = subprocess.Popen(['mcrun ' + self.__instrFile + ' --info'], 
                                   stdout=subprocess.PIPE, 
                                   stderr=subprocess.STDOUT,
                                   shell=True)
        # get std out info
        info = []
        while process.poll() == None:
            for l in process.stdout:
                info.append(l.rstrip('\n'))
            time.sleep(0.1)
        process.wait()
        
        # get parameters from info
        params = []
        for l in info:
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
        self.view = McView()
        self.state = McGuiState()
        self.connectAllCallbacks()    
        self.state.initState()
        self.view.showMainWindow()
    
    ''' UI callbacks
    '''
    def handleRunSim(self):
        instr_params = self.state.getInstrParams()
        fixed_params, new_instr_params = self.view.showStartSimDialog(instr_params)
        
        if fixed_params != None:
            self.state.run(fixed_params, new_instr_params)
        
    def handleOpenInstrument(self):
        instrFile = self.view.showOpenInstrumentDlg(self.state.getWorkDir())
        if instrFile:
            self.state.loadInstument(instrFile)        
    
    def handleChangeWorkDir(self):
        workDir = self.view.showChangeWorkDirDlg(self.state.getWorkDir())
        if workDir:
            self.state.setWorkDir(workDir)
    
    def handleExit(self):
        sys.exit()
    
    def handlePlotResults(self):
        print("not implemented")
        
    def handleHelpWeb(self):
        '''open the mcstas homepage'''
        mcurl = 'http://www.mcstas.org'
        webbrowser.open_new_tab(mcurl)
    
    def handleHelpPdf(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ..)
        mcman = '/usr/share/mcstas/2.1/doc/manuals/mcstas-manual.pdf'
        webbrowser.open_new_tab(mcman)
    
    def handleHelpPdfComponents(self):
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  ...)
        mcman = '/usr/share/mcstas/2.1/doc/manuals/mcstas-components.pdf'
        webbrowser.open_new_tab(mcman)
    
    def handleHelpAbout(self):
        print("not implemented")
        
    def handleEditInstrument(self):
        instr = self.state.getInstrumentFile()
        self.view.showCodeEditorWindow(instr)
        
    def handleCloseInstrument(self):
        self.state.loadInstument('', unload=True)
        self.view.closeCodeEditorWindow()
    
    def handleSaveInstrument(self, text):
        result = self.state.saveInstrumentIfFileExists(text)
        if result:
            self.view.ew.assumeDataSaved()
            
    def handleSaveAs(self):
        oldinstr = self.state.getInstrumentFile()
        if oldinstr != '':
            newinstr = self.view.showSaveAsDialog(oldinstr)
        
        if newinstr != '':
            text = self.state.getInstrContents()
            self.state.createNewInstrumentFile(newinstr, text)
            self.state.loadInstument(newinstr)
    
    def handleNewInstrument(self):
        newinstr = self.view.showNewInstrDialog(self.state.getWorkDir())
        instr = self.state.createNewInstrumentFile(newinstr, '')
        if instr != '':
            self.state.loadInstument(instr)
    
    ''' Connect UI and state callbacks 
    '''
    def connectAllCallbacks(self):        
        # connect UI widget signals to our handlers/logics
        # NOTICE: This explicit widget access is exceptional - all widget access is otherwise handled by the view classes
        
        mwui = self.view.mwui
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
        mwui.actionRun_Simulation.triggered.connect(self.state.run)
        
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        mwui.actionMcstas_Component_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        ew = self.view.ew
        ew.saveRequest.connect(self.handleSaveInstrument)
        
        st = self.state
        st.simStateUpdated.connect(self.view.updateSimState)
        st.instrumentUpdated.connect(self.view.updateInstrumentLabel)
        st.statusUpdated.connect(self.view.updateStatus)
        st.logMessage.connect(self.view.updateLog)
        
        
''' Program execution
'''
def main():
    mcguiApp = QtGui.QApplication(sys.argv)
    mcguiApp.ctr = McGuiAppController()
    
    sys.exit(mcguiApp.exec_())

if __name__ == '__main__':
    main()

