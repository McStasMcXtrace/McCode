#!/usr/bin/env python

'''mgcui 

Initial version written during Winter/spring of 2015

@author: jaga
'''
import sys
import os
import webbrowser
from PyQt4 import QtGui
from PyQt4 import QtCore
from viewclasses import McView


''' State
Holds unique state values.
'''
class McGuiState(QtCore.QObject):
    instrFile = ''
    instrFileUpdated = QtCore.pyqtSignal(QtCore.QString)
    workDir = './'
    workDirUpdated = QtCore.pyqtSignal(QtCore.QString)
    status = ''
    statusUpdated = QtCore.pyqtSignal(QtCore.QString)
    
    def loadInstument(self, instrFile):
        self.instrFile = instrFile
        self.instrFileUpdated.emit(self.instrFile) 
        self.setStatus("Instrument: " + instrFile)
    
    def setWorkDir(self, workDir):
        self.workDir = workDir
        self.workDirUpdated.emit(self.workDir)
        self.setStatus("Work dir: " + workDir)
    
    def setStatus(self, status):
        self.status = status
        self.statusUpdated.emit(self.status)
        
    def initState(self):
        self.setWorkDir(os.getcwd())
        
    
''' Controller
Implements ui and data callbacks.
'''
class McGuiAppController():
    
    ''' UI callbacks
    '''
    def handleLoadInstrument(self):
        instrFile = self.view.showOpenInstrumentDlg()
        if instrFile:
            self.state.loadInstument(instrFile)
            
    def handleChangeWorkDir(self):
        workDir = self.view.showChangeWorkDirDlg()
        if workDir:
            self.state.setWorkDir(workDir)
        
    def handleExit(self):
        sys.exit()
    
    def handleRunSim(self):
        print("not implemented")
        
    def handleCompileSim(self):
        print("not implemented")
        
    def handlePlotResults(self):
        print("not implemented")
        
    def handleSetWorkDir(self):
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
        
    ''' Connect ui callbacks 
    '''
    def setupView(self):
        # setup view
        self.view = McView()
        
        # connect UI widget signals to our handlers/logics
        # WARNING: NEVER update ui widget state from this class, always delegate to view. This explicit widget access is AN EXCEPTION
        mwui = self.view.mwui
        mwui.actionQuit.triggered.connect(self.handleExit)
        mwui.actionOpen_instrument.triggered.connect(self.handleLoadInstrument)
        mwui.actionChange_Working_Dir.triggered.connect(self.handleChangeWorkDir)
        
        mwui.tbtnInstrument.clicked.connect(self.handleLoadInstrument)
        mwui.tbtnWorkDir.clicked.connect(self.handleChangeWorkDir)
        
        mwui.actionCompile_Instrument.triggered.connect(self.handleCompileSim)
        mwui.actionRun_Simulation.triggered.connect(self.handleRunSim)
        
        mwui.actionPS.triggered.connect(self.handlePlotResults)
        mwui.actionCS.triggered.connect(self.handleCompileSim)
        mwui.actionRS.triggered.connect(self.handleRunSim)
        
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        mwui.actionMcstas_Component_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        self.view.initMainWindow()
    
    ''' Connect State callbacks.
    ''' 
    def setupState(self):
        self.state = McGuiState()
        st = self.state
        st.instrFileUpdated.connect(self.view.updateInstrumentFile)
        st.workDirUpdated.connect(self.view.updateWorkDir)
        st.statusUpdated.connect(self.view.updateStatus)
        st.initState()

''' Program execution
'''
def main():
    mcguiApp = QtGui.QApplication(sys.argv)
    
    ctr = McGuiAppController()
    ctr.setupView()
    ctr.setupState()
    ctr.view.showMainWindow()
    
    sys.exit(mcguiApp.exec_())

if __name__ == '__main__':
    main()

