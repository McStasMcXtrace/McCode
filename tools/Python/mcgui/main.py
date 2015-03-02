'''mgcui 

Initial version written during Winter/spring of 2015

@author: jaga
'''
import sys
import webbrowser
from PyQt4 import QtGui
from PyQt4 import QtCore
from viewclasses import McView

''' State
Holds unique state values.
'''
class McGuiState(QtCore.QObject):
    instrumentFile = ''
    instrumentUpdatedSignal = QtCore.pyqtSignal()
    workingDirectory = './'
    
    def loadInstument(self, instrumentFile):
        self.instrumentFile = instrumentFile
        self.instrumentUpdatedSignal.emit()
    
    
''' Controller
Implements ui and data callbacks.
'''
class McGuiAppController():
    
    ''' UI callbacks
    '''
    def handleLoadInstrument(self):
        instrFile = self.view.showOpenInstrumentDlg()
        self.state.loadInstument(instrFile)
        
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
        # TODO: make it cross-platform (e.g. os.path.realpath(__file__) +  etc.)
        mcman = '/usr/share/mcstas/2.1/doc/manuals/mcstas-manual.pdf'
        webbrowser.open_new_tab(mcman)
        
    def handleHelpPdfComponents(self):
        print("not implemented")
        
    def handleHelpAbout(self):
        print("not implemented")
        
    def setupView(self):
        # setup view
        self.view = McView()
        
        # connect ui widget signals to our handlers/logics
        # WARNING: NEVER update ui widget state from this class, always delegate to view! This explicit widget access is AN EXCEPTION
        mwui = self.view.mwui
        mwui.actionQuit.triggered.connect(self.handleExit)
        mwui.actionOpen_instrument.triggered.connect(self.handleLoadInstrument)
        mwui.tbtnInstrument.clicked.connect(self.handleLoadInstrument)
        mwui.actionCompile_Instrument.triggered.connect(self.handleCompileSim)
        mwui.actionRun_Simulation.triggered.connect(self.handleRunSim)
        mwui.actionPS.triggered.connect(self.handlePlotResults)
        mwui.actionCS.triggered.connect(self.handleCompileSim)
        mwui.actionRS.triggered.connect(self.handleRunSim)
        mwui.actionMcstas_Web_Page.triggered.connect(self.handleHelpWeb)
        mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdf)
        # TODO: mwui.actionMcstas_User_Manual.triggered.connect(self.handleHelpPdfComponents)
        mwui.actionAbout.triggered.connect(self.handleHelpAbout)
        
        self.view.showMainWindow()
    
    def updateInstrumentFileView(self):
        self.view.updateStatus("Loaded instrument file: " + str(self.state.instrumentFile))
        self.view.updateInstrumentFile(self.state.instrumentFile)
       
    ''' Internal state callbacks. NOTE: Always delegate state change reflections to view
    ''' 
    def setupState(self):
        # setup state
        self.state = McGuiState()
        st = self.state
        st.instrumentUpdatedSignal.connect(self.updateInstrumentFileView)
    

''' Program execution
'''
def main():
    mcguiApp = QtGui.QApplication(sys.argv)
    
    ctr = McGuiAppController()
    ctr.setupState()
    ctr.setupView()
    
    sys.exit(mcguiApp.exec_())

if __name__ == '__main__':
    main()

