''' 
mcgui UI.

@author: jaga
'''
import os
import mccode_config
import re
from widgets import *
from mcguiutils import McGuiUtils
from mcfileutils import McComponentParser
from PyQt4 import Qsci


''' View class containing all windows and dialogs.
ALL explicit ui widget updates MUST be handled by this class
'''
class McView(object):
    def __init__(self):
        # create main window
        self.mw = McMainWindow()
        self.mw.ui.lblInstrument.setText("")
        self.ew = McCodeEditorWindow()
        # a hack to enable mw to close ew
        self.mw.ew = self.ew
    
    def initMainWindowDynamicElements(self, args, callback):
        self.mw.initDynamicView(args, callback)
        
    def initCodeEditorComponentMenu(self, args):
        self.ew.initComponentMenu(args)
     
    def showMainWindow(self):
        self.mw.show()

    def showCodeEditorWindow(self, instr=''):
        self.ew.initCodeEditor(instr)
        self.ew.show()
    
    def closeCodeEditorWindow(self):
        return self.ew.close()
    
    ''' Update UI data
    '''
    def updateInstrument(self, labels):
        ''' labels: <instrument path>, <work dir> '''
        self.mw.ui.lblInstrument.setText(labels[0])
        if str(labels[0]) == '':
            self.__ssd = None
        
    def updateStatus(self, text=''):
        self.mw.ui.statusbar.showMessage(text)
        
    def updateLog(self, text='', error=False):
        if error:
            self.mw.ui.txtbrwMcgui.setTextColor(QtGui.QColor('red'))
        else:
            self.mw.ui.txtbrwMcgui.setTextColor(QtGui.QColor('black'))
        self.mw.ui.txtbrwMcgui.append(text)
            
    def updateSimState(self, state=[]):
        enableRun = state[0] == 'True'
        enablePlot = state[1] == 'True'
        enableInterrupt = False;
        if len(state)>2:
            enableInterrupt = state[2] == 'True'
        
        # clear start simulation dialog
        if not enableRun:
            self.__ssd = None
        
        # set enabled/disabled states on menus and buttons
        ui = self.mw.ui
        ui.btnRun.setEnabled(enableRun)
        ui.btnEdit.setEnabled(enableRun)
        ui.btnPlot.setEnabled(enablePlot)
        if enableRun:
            ui.lblInstrument.setStyleSheet('color: black')
        else:
            ui.lblInstrument.setStyleSheet('color: red')
        ui.actionClose_Instrument.setEnabled(enableRun)
        ui.actionPlot.setEnabled(enablePlot)
        ui.actionRun_Simulation.setEnabled(enableRun)
        ui.actionSave_As.setEnabled(enableRun)
        ui.actionOpen_instrument.setEnabled(True)
        ui.actionNew_Instrument.setEnabled(True)
        ui.menuNew_From_Template.setEnabled(True)
        ui.actionEdit_Instrument.setEnabled(enableRun)
        ui.actionCompile_Instrument.setEnabled(enableRun)
        ui.actionCompile_Instrument_MPI.setEnabled(enableRun)
        
        # set action of run button:
        if enableInterrupt: 
            ui.btnRun.setText('Halt')
            ui.btnRun.setToolTip('Interrupt current simulation')
            ui.actionRun_Simulation.setEnabled(False)
            ui.actionCompile_Instrument.setEnabled(False)
            ui.actionCompile_Instrument_MPI.setEnabled(False)
            ui.actionClose_Instrument.setEnabled(False)
            ui.actionSave_As.setEnabled(False)
            ui.actionOpen_instrument.setEnabled(False)
            ui.actionNew_Instrument.setEnabled(False)
            ui.menuNew_From_Template.setEnabled(False)
        else: 
            ui.btnRun.setText('Run...')
            ui.btnRun.setToolTip('')
        
    ''' UI actions
    '''
    def showOpenInstrumentDlg(self, lookDir):
        dlg = QtGui.QFileDialog()
        dlg.setDirectory(lookDir)
        dlg.setNameFilter(mccode_config.configuration["MCCODE"]+" instruments (*.instr)");
        if dlg.exec_():
            return dlg.selectedFiles()[0]
    
    def showChangeWorkDirDlg(self, lookDir):
        dlg = QtGui.QFileDialog()
        dlg.setFileMode(QtGui.QFileDialog.Directory)
        dlg.setDirectory(lookDir)
        if dlg.exec_():
            return dlg.selectedFiles()[0]

    def showStartSimDialog(self, params):
        if self.__ssd == None:
            self.__ssd = McStartSimDialog()
        self.__ssd.createParamsWidgets(params)
        if self.__ssd.exec_():
            return self.__ssd.getValues()
        else: 
            return None, None
    
    def showNewInstrDialog(self, lookdir):
        dlg = QtGui.QFileDialog()
        dlg.setDirectory(lookdir)
        dlg.setNameFilter(mccode_config.configuration["MCCODE"]+" instruments (*.instr)");
        return dlg.getSaveFileNameAndFilter(parent=None, caption=QtCore.QString('Create Instrument file...'))[0]
        
    
    def showNewInstrFromTemplateDialog(self, instr):
        dlg = QtGui.QFileDialog()
        return dlg.getSaveFileNameAndFilter(parent=None, caption=QtCore.QString('Create Instrument file from Template...'), directory=instr)[0]
        
    def showSaveAsDialog(self, instr):
        dlg = QtGui.QFileDialog()
        dlg.setFileMode(QtGui.QFileDialog.AnyFile)
        return dlg.getSaveFileNameAndFilter(parent=None, caption=QtCore.QString('Save Instrument As...'), directory=instr)[0]

    def showConfigDialog(self):
        dlg = McConfigDialog()
        dlg.initConfigData(None)
        dlg.exec_()
        
    def showAboutBox(self, text):
        if mccode_config.configuration["MCCODE"] == "mcstas":
            prefix = "mc"
        else:
            prefix = "mx"
        QtGui.QMessageBox.about(self.mw, prefix+'gui-py: About', text)
            

''' Main Window widgets wrapper class
Events callbacks are hooked elsewhere.
'''
class McMainWindow(QtGui.QMainWindow):
    def __init__(self, parent=None): 
        super(McMainWindow, self).__init__(parent)
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.ui.dynamicMenuClicked = QtCore.pyqtSignal(QtCore.QString)
    
        # set main window title depending on flavour
        prefix = 'mx'
        if mccode_config.configuration["MCCODE"] == "mcstas": 
            prefix = 'mc'
        self.setWindowTitle(prefix + 'gui-py') 
        self.ui.actionMcdoc.setText(prefix + "doc Component Reference")
        self.ui.actionMcstas_User_Manual.setText(prefix + "stas User Manual")
        self.ui.actionMcstas_Component_Manual.setText(prefix + "stas Component Manual")
        self.ui.actionMcstas_Web_Page.setText(prefix + "stas Web Page")
        
    def initDynamicView(self, args, callback):
        ''' - args ([str, [], []]): list of triplets consisting of site name, 
                                    [instrument names], [instrument file paths] 
            - callback (func(str)): function which takes a single string parameter, call with full path 
                                    name of selected instrument 
        '''
        self.ui.menuNew_From_Template.clear()
        
        for i in range(len(args)):
            site = args[i][0]
            instrs = args[i][1]
            instrs_fulpath = args[i][2]
            
            menu = self.ui.menuNew_From_Template.addMenu(site)
            
            for j in range(len(instrs)):
                action = menu.addAction(instrs[j])
                action.triggered[()].connect(lambda item=instrs_fulpath[j]: callback(item))
    
    def closeEvent(self, event):
        ''' allow close down only if editor window did not reject '''
        if not self.ew.close():
            event.ignore()
    
        
''' Code editor window widgets wrapper class
'''
class McCodeEditorWindow(QtGui.QMainWindow):    
    volatileDataExists = False
    volatileDataTransition = QtCore.pyqtSignal(bool)
    saveRequest = QtCore.pyqtSignal(QtCore.QString)
    
    def __init__(self, parent=None):
        super(McCodeEditorWindow, self).__init__(parent)
        self.ui =  Ui_EditorWindow()
        self.ui.setupUi(self)
        self.__initScintilla()
        self.__initCallbacks()
    
    def initComponentMenu(self, args):
        ''' args - [category, comp_names[], comp_parsers[]]
        '''
        all_comp_names = []
        for i in range(len(args)):
            category = args[i][0]
            comp_names = args[i][1]
            for name in comp_names:
                all_comp_names.append(name)
            comp_parsers = args[i][2]
            
            menu = self.ui.menuInsert.addMenu(category)
            
            for j in range(len(comp_names)):
                action = menu.addAction(comp_names[j])
                action.triggered[()].connect(lambda comp_parser=comp_parsers[j]: self.__handleComponentClicked(comp_parser))
        
        self.setLexerComps(self.__scintilla.__myApi, all_comp_names)
        
    def initCodeEditor(self, instr):
        if instr != '':
            self.__scintilla.setText(open(instr).read())
        else:
            self.__scintilla.setText('')
        self.setWindowTitle(mccode_config.configuration["MCCODE"] + ": " + instr)
        self.assumeDataSaved()
        
    def assumeDataSaved(self):
        self.volatileDataTransition.emit(False)
    
    def save(self):
        ''' external save text hook '''
        self.__handleSaveAction()
 
    def closeEvent(self, event):
        ''' hook to display a "save changes?" dialog if there are unsaved changes 
        '''
        if self.volatileDataExists:
            reply = QtGui.QMessageBox.question(self, 
                                               'The instrument has been modified.', 
                                               'Do you want to save changes?',
                                               'Save',      # default button, reply == 0
                                               'Discard',   # reply == 1
                                               'Cancel')    # reply == 2
            if reply == 0:
                self.saveRequest.emit(self.__scintilla.text())
                self.assumeDataSaved()
                event.accept()
            elif reply == 1:
                event.accept()
            elif reply == 2:
                event.ignore()
        else:
            event.accept()
            
    def __handleComponentClicked(self, comp_parser):
        dlg = McInsertComponentDialog()
        dlg.initComponentData(comp_parser)
        if dlg.exec_():
            comp_type, inst_name, params, atrel = dlg.getValues()
        else: 
            return
        
        text = "COMPONENT " + inst_name + " = " + comp_type + "( "
        i_max = len(params)-1
        for i in range(len(params)):
            p = params[i]
            text += "\n    " + p[0] + "=" + p[1]
            if i < i_max:
                text += ", "

        text += ")"
        text += "\nAT (" + atrel[0] + ", " + atrel[1] + ", " + atrel[2] + ") RELATIVE " + atrel[3] 
        text += "\nROTATED (" + atrel[4] + ", " + atrel[5] + ", " + atrel[6] + ") RELATIVE " + atrel[7]
        
        self.__scintilla.insert(text)
        
        # set cursor position
        i, j = self.__scintilla.getCursorPosition()
        pos = self.__scintilla.positionFromLineIndex(i, j)
        k, l = self.__scintilla.lineIndexFromPosition(pos + len(text))
        self.__scintilla.setCursorPosition(k, l)
        
    def __initScintilla(self):
        # delete text editor placeholder 
        scintilla = Qsci.QsciScintilla(self)
        
        ########################
        # setup scintilla
        # set default font
        font = QtGui.QFont()
        font.setFamily('Deja Vu Sans Mono')
        font.setFixedPitch(True)
        font.setPointSize(11)
        
        # brace matching
        scintilla.setBraceMatching(Qsci.QsciScintilla.SloppyBraceMatch)
        
        # set lexer
        lexer = Qsci.QsciLexerCPP()
        lexer.setDefaultFont(font)
        lexer.setFont(font)
        scintilla.setLexer(lexer)
        
        scintilla.setLexer(lexer)
        scintilla.__myLexer = lexer # save reference to retain scope
        
        # auto-completion api
        scintilla.__myApi = Qsci.QsciAPIs(lexer)
        
        scintilla.setAutoCompletionThreshold(1)
        scintilla.setAutoCompletionSource(Qsci.QsciScintilla.AcsAPIs)
        
        # remove horizontal scrollbar
        scintilla.SendScintilla(Qsci.QsciScintilla.SCI_SETHSCROLLBAR, 0)
        ########################
        
        # insert widget
        self.setCentralWidget(scintilla)
        self.__scintilla = scintilla
    
    @staticmethod
    def setLexerComps(api, all_comp_names):
        api.clear()
        
        # add mcstas meta keywords
        api.add("ABSOLUTE")
        api.add("AT")
        api.add("COMPONENT")
        api.add("DECLARE")
        api.add("DEFINE")
        api.add("DEFINITION")
        api.add("END")
        api.add("MCDISPLAY")
        api.add("FINALLY")
        api.add("INITIALIZE")
        api.add("INSTRUMENT")
        api.add("OUTPUT")
        api.add("PARAMETERS")
        api.add("RELATIVE")
        api.add("ROTATED")
        api.add("PREVIOUS")
        api.add("SETTING")
        api.add("STATE")
        api.add("POLARISATION")
        api.add("TRACE")
        api.add("SHARE")
        api.add("EXTEND")
        api.add("GROUP")
        api.add("SAVE")
        api.add("JUMP")
        api.add("WHEN")
        api.add("NEXT")
        api.add("ITERATE")
        api.add("MYSELF")
        api.add("COPY")
        api.add("SPLIT")
        api.add("REMOVABLE")
        api.add("DEPENDENCY")
        # add components
        for name in all_comp_names:
            api.add(name)
        
        api.prepare()
            
    def __initCallbacks(self):
        # connect menu items to corresponding scintilla slots
        ui = self.ui 
        ui.actionUndo.triggered.connect(self.__scintilla.undo)
        ui.actionRedo.triggered.connect(self.__scintilla.redo)
        ui.actionSelect_All.triggered.connect(lambda: self.__scintilla.selectAll()) # why is l. expr. needed here?
        ui.actionCopy.triggered.connect(self.__scintilla.copy)
        ui.actionCut.triggered.connect(self.__scintilla.cut)
        ui.actionPaste.triggered.connect(self.__scintilla.paste)
        ui.actionSave.triggered.connect(self.__handleSaveAction)
        ui.actionClose_Instrument_Editor.triggered.connect(self.close)
        ui.actionComponent_Browser.triggered.connect(self.__handleComponentBrowser)
        ui.actionInsert_Header.triggered.connect(self.__handleInsertHeader)
        ui.actionInsert_Body.triggered.connect(self.__handleInsertBody)
        
        # connect "text changed" signal to our handler to detect unsaved changes
        self.__scintilla.textChanged.connect(self.__handleTextChanged)
        
        self.volatileDataTransition.connect(self.__handleVolatileDataPresent)
    
    def __handleInsertHeader(self):
        text = open(os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "examples", "template_header_simple.instr")).read()
        
        # insert template header and set cursor position to be after header text
        self.__scintilla.setCursorPosition(0, 0)
        self.__scintilla.insert(text)
        k, l = self.__scintilla.lineIndexFromPosition(len(text))
        self.__scintilla.setCursorPosition(k, l)
    
    def __handleInsertBody(self):
        text = open(os.path.join(mccode_config.configuration["MCCODE_LIB_DIR"], "examples", "template_body_simple.instr")).read()
        
        # insert template body at cursor position and reposition curser to be after body text
        i, j = self.__scintilla.getCursorPosition()
        pos = self.__scintilla.positionFromLineIndex(i, j)
        self.__scintilla.insert(text)
        k, l = self.__scintilla.lineIndexFromPosition(pos + len(text))
        self.__scintilla.setCursorPosition(k, l)
    
    def __handleComponentBrowser(self):
        dlg = QtGui.QFileDialog()
        dlg.setDirectory(mccode_config.configuration["MCCODE_LIB_DIR"])
        dlg.setNameFilter(mccode_config.configuration["MCCODE"]+"component files (*.comp)");
        if dlg.exec_():
            comp_file = dlg.selectedFiles()[0]
            parser = McComponentParser(comp_file)
            self.__handleComponentClicked(parser)
    
    def __handleTextChanged(self):
        if not self.volatileDataExists:
            self.volatileDataTransition.emit(True)
    
    def __handleSaveAction(self):
        self.saveRequest.emit(self.__scintilla.text())
        
    def __handleVolatileDataPresent(self, volatileDataExists=False):
        if volatileDataExists:
            title = self.windowTitle()
            self.setWindowTitle('*' + title)
        else:
            title = str(self.windowTitle())
            self.setWindowTitle(title.replace('*', ''))
        self.volatileDataExists = volatileDataExists
        self.ui.actionSave.setEnabled(volatileDataExists)


''' Start simulation widgets wrapper class
Programatically alters the dialog to match current instrument.
Supports reuse of widgets from sim to sim, to retain input values.
Works as a dialog - call _exec(), probe for return behavior and
state to proceed.
'''
class McStartSimDialog(QtGui.QDialog):
    def __init__(self, parent=None):
        super(McStartSimDialog, self).__init__(parent)
        self.ui = Ui_dlgStartSim()
        self.ui.setupUi(self)
        self.ui.btnStart.clicked.connect(self.accept)
        self.ui.btnCancel.clicked.connect(self.reject)
        
    def getValues(self):
        ''' Return values:
        
            fixed_params[]:
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
        # simulation or trace option
        p0 = None
        if self.ui.cbxSimTrace.currentIndex() == 0:
            p0 = SimTraceEnum.SIM
        else:
            p0 = SimTraceEnum.TRACE
        
        # neutron count
        p1 = self.ui.edtNeutronCnt.text()
        
        # steps
        p2 = self.ui.edtSteps.text()
        
        # gravity
        p3 = self.ui.cbxGravity.currentIndex() == 1
        
        # clustering option 
        p4 = None
        if self.ui.cbxClustering.currentIndex() == 0:
            p4 = ClusteringEnum.SINGLE
        if self.ui.cbxClustering.currentIndex() == 1:
            p4 = ClusteringEnum.MPI
        if self.ui.cbxClustering.currentIndex() == 2:
            p4 = ClusteringEnum.MPI_RC
            
        # clustring option
        p5 = self.ui.edtNodes.text()

        # seed
        p6 = self.ui.edtRandomSeed.text()
        
        # output dir
        p7 = str(self.ui.edtOutputDir.text())
        
        fixed_params =[p0, p1, p2, p3, p4, p5, p6, p7]
        
        # get dynamic params
        params = []
        for w in self.__wParams:
            p = []
            p.append(str(w[0].text()).rstrip(':'))
            p.append(str(w[1].text()))
            params.append(p)
        
        return fixed_params, params
    
    __wParams = []
    __oldParams = []
    def createParamsWidgets(self, params):
        
        # this logics keeps params values of existing/previous non-dummy widgets, for value reuse 
        self.__oldParams = []
        for w in self.__wParams:
            old_name = 'no_re_match'
            name_match = re.search('(.*):', w[0].text())
            if name_match:
                old_name = name_match.group(1)
            old_value = w[1].text()
            self.__oldParams.append([old_name, old_value])
        
        # clear the containing grid
        grd = self.ui.gridGrid
        for i in reversed(range(grd.count())): 
            grd.itemAt(i).widget().setParent(None)
        
        # prepare new params widgets
        self.__wParams = []
        
        # insert custom params widgets
        i = -1
        x = 0
        y = 0
        p_index = 0
        for p in params:
            # get param name, value
            name = QtCore.QString(p[0])
            value = QtCore.QString(p[1])
            
            # reuse old param values, if matching position in grid (p_index) and param name
            if len(self.__oldParams) > p_index:
                old_name = self.__oldParams[p_index][0]
                old_value = self.__oldParams[p_index][1]
                if str(old_name) == str(name):
                    value = QtCore.QString(old_value)
            
            i = i + 1
            x = i % 6
            y = i / 6
            
            lbl = QtGui.QLabel(self.ui.gbxGrid)
            lbl.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
            lbl.setObjectName("lbl" + name)
            lbl.setText(name + ':')
            self.ui.gridGrid.addWidget(lbl, y, x, 1, 1)
            
            i = i + 1
            x = i % 6
            
            edt = QtGui.QLineEdit(self.ui.gbxGrid)
            edt.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
            edt.setObjectName("edt" + name)
            edt.setText(value)
            self.ui.gridGrid.addWidget(edt, y, x, 1, 1)
            
            self.__wParams.append([lbl, edt])
            
            p_index += 1
            
        self.ui.btnStart.setFocus()

class SimTraceEnum:
    SIM = 0
    TRACE = 1

class ClusteringEnum:
    SINGLE = 0 
    MPI = 1
    MPI_RC = 2


''' Start simulation widgets wrapper class
Programatically alters the dialog to match current instrument.
Supports reuse of widgets from sim to sim, to retain input values.
Works as a dialog - call _exec(), probe for return behavior and
state to proceed.
'''
class McInsertComponentDialog(QtGui.QDialog):
    __standard_le_style = None
    def __init__(self, parent=None):
        super(McInsertComponentDialog, self).__init__(parent)
        self.ui = Ui_dlgInsertComponent()
        self.ui.setupUi(self)
        self.ui.btnInsert.clicked.connect(self.accept)
        self.ui.btnCancel.clicked.connect(self.reject)
        self.__standard_le_style = self.ui.edtInstanceName.styleSheet()
        
    def accept(self):
        # detect missing default values
        dirty = False
        
        # mark/unmark params dynamic lineedits
        first_params_hit = True
        for w in self.__wParams:
            if w[1].text() == '':
                w[1].setStyleSheet("border: 3px solid red;")
                dirty = True
                if first_params_hit:
                    w[1].setFocus()
                    first_params_hit = False
            else:
                w[1].setStyleSheet(self.__standard_le_style)
        
        # mark/unmark instance name lineedit
        if self.ui.edtInstanceName.text() == '':
            self.ui.edtInstanceName.setStyleSheet("border: 3px solid red;")
            if not dirty:
                self.ui.edtInstanceName.setFocus()
            dirty = True
        else:
            self.ui.edtInstanceName.setStyleSheet(self.__standard_le_style)
                
        # exit if all lineedit text boxes are filled out 
        if not dirty:
            super(McInsertComponentDialog, self).accept()
    
    __wParams = []
    def initComponentData(self, comp_parser):
        # parse component info
        comp_parser.parse()
        
        # window title
        self.setWindowTitle("Component: " + comp_parser.name)
        
        # info & description docstrings - make sure newlines work in case doc includes html
        info_description = comp_parser.info + '\n\n' + comp_parser.description
        info_description_html = str(info_description).replace('\n', '<br>')
        self.ui.lblDescr.setText(info_description_html)
        
        # clear params grd
        grd = self.ui.gridParameters
        for i in reversed(range(grd.count())): 
            grd.itemAt(i).widget().setParent(None)
        
        # populate and init params grd
        self.__wParams = None
        self.__wParams = []
        for i in range(len(comp_parser.pars)):
            par = McComponentParser.McComponentParInfo(comp_parser.pars[i])
            
            # i'th line/row of the UI
            y = i
            
            # parameter name label
            x = 0
            lbl = QtGui.QLabel()
            lbl.setObjectName("lbl" + par.par_name)
            lbl.setText(par.par_name + ':')
            self.ui.gridParameters.addWidget(lbl, y, x, 1, 1)
            
            # parameter value line-edit
            x = 1
            edt = QtGui.QLineEdit()
            edt.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
            edt.setObjectName("edt" + par.par_name)
            edt.setText(par.default_value)
            self.ui.gridParameters.addWidget(edt, y, x, 1, 1)
            
            # save name, value widget references for use in self.getValues
            self.__wParams.append([lbl, edt])
            
            # parameter docstring label
            x = 2
            lbl = QtGui.QLabel()
            lbl.setWordWrap(True)
            lbl.setObjectName("lbl" + par.par_name + "_doc")
            lbl.setText(par.doc_and_unit)
            self.ui.gridParameters.addWidget(lbl, y, x, 1, 1)
            
        # fix tab-order
        q = self.ui.btnInsert 
        for i in range(len(self.__wParams)):
            w = self.__wParams[i][1]
            self.setTabOrder(q, w)
            q = w
        self.setTabOrder(q, self.ui.edtAtX)
            
        # init instance-name field with an example, mark the text
        tbx = self.ui.edtInstanceName
        tbx.setText(str.lower(comp_parser.name))
        tbx.setFocus()
        tbx.selectAll()
        
    def getValues(self):
        ''' 
        inst_name : contents of instance name field 
        params : list of [name, value] pairs matching component parameters
        '''
        # instance name
        inst_name = self.ui.edtInstanceName.text()
        comp_type = str(self.windowTitle()).lstrip('Component: ')
        
        # get dynamic params
        params = []
        for w in self.__wParams:
            p = []
            p.append(str(w[0].text()).rstrip(':'))
            p.append(str(w[1].text()))
            params.append(p)
        
        # get values for AT(x,y,z), RELATIVE <posrel>, ROTATED(x,y,z), RELATIVE <rotrel> 
        atrel = []
        atrel.append(self.ui.edtAtX.text())
        atrel.append(self.ui.edtAtY.text())
        atrel.append(self.ui.edtAtZ.text())
        atrel.append(self.ui.edtAtRel.text())
        atrel.append(self.ui.edtRotX.text())
        atrel.append(self.ui.edtRotY.text())
        atrel.append(self.ui.edtRotZ.text())
        atrel.append(self.ui.edtRotRel.text())
        
        return comp_type, inst_name, params, atrel


''' mcgui config widgets wrapper class
'''
class McConfigDialog(QtGui.QDialog):
    __standard_le_style = None
    def __init__(self, parent=None):
        super(McConfigDialog, self).__init__(parent)
        self.ui = Ui_dlgConfig()
        self.ui.setupUi(self)
        
        self.ui.btnOk.clicked.connect(self.accept)
        self.ui.btnSave.clicked.connect(self.save)
        self.ui.btnCancel.clicked.connect(self.reject)
    
    def initConfigData(self, args):
        # comboboxes
        mcrun_lst, mcplot_lst, mcdisplay_lst = McGuiUtils.getMcCodeConfigOptions(mccode_config.configuration["MCCODE"])
        
        # mcrun combobox
        selected_val = mccode_config.configuration["MCRUN"]
        i = 0
        for val in mcrun_lst: 
            self.ui.cbxMcrun.addItem(val)
            if val == selected_val:
                self.ui.cbxMcrun.setCurrentIndex(i)
            i += 1
        self.ui.cbxMcrun.conf_var = "MCRUN"
        self.ui.cbxMcrun.conf_org_value = mccode_config.configuration["MCRUN"]
        self.ui.cbxMcrun.conf_options_lst = mcrun_lst
        
        # mcplot combobox
        selected_val = mccode_config.configuration["MCPLOT"]
        i = 0
        for val in mcplot_lst: 
            self.ui.cbxMcPlot.addItem(val)
            if val == selected_val:
                self.ui.cbxMcPlot.setCurrentIndex(i)
            i += 1
        self.ui.cbxMcPlot.conf_var = "MCPLOT"
        self.ui.cbxMcPlot.conf_org_value = mccode_config.configuration["MCPLOT"]
        self.ui.cbxMcPlot.conf_options_lst = mcplot_lst
        
        # mcdisplay combobox
        selected_val = mccode_config.configuration["MCDISPLAY"]
        i = 0
        for val in mcdisplay_lst: 
            self.ui.cbxMcdisplay.addItem(val)
            if val == selected_val:
                self.ui.cbxMcdisplay.setCurrentIndex(i)
            i += 1
        self.ui.cbxMcdisplay.conf_var = "MCDISPLAY"
        self.ui.cbxMcdisplay.conf_org_value = mccode_config.configuration["MCDISPLAY"]
        self.ui.cbxMcdisplay.conf_options_lst = mcdisplay_lst
        
        # line edits
        self.ui.edtCC.setText(mccode_config.compilation["CC"])
        self.ui.edtCC.conf_var = "CC"
        
        self.ui.edtCflags.setText(mccode_config.compilation["CFLAGS"])
        self.ui.edtCflags.conf_var = "CFLAGS"
        
        self.ui.edtMpicc.setText(mccode_config.compilation["MPICC"])
        self.ui.edtMpicc.conf_var = "MPICC"
        
        self.ui.edtMPIrun.setText(mccode_config.compilation["MPIRUN"])
        self.ui.edtMPIrun.conf_var = "MPIRUN"
        
        self.ui.edtNumNodes.setText(mccode_config.compilation["MPINODES"])
        self.ui.edtNumNodes.conf_var = "MPINODES"
    
    def __pullValuesTo_mccode_config(self):
        # mcrun combobox
        i = self.ui.cbxMcrun.currentIndex()
        mccode_config.configuration["MCRUN"] = self.ui.cbxMcrun.conf_options_lst[i]
        
        # mcrun combobox
        i = self.ui.cbxMcPlot.currentIndex()
        mccode_config.configuration["MCPLOT"] = self.ui.cbxMcPlot.conf_options_lst[i]
        
        # mcrun combobox
        i = self.ui.cbxMcdisplay.currentIndex()
        mccode_config.configuration["MCDISPLAY"] = self.ui.cbxMcdisplay.conf_options_lst[i]
        
        # line edits
        mccode_config.compilation[str(self.ui.edtCC.conf_var)] = str(self.ui.edtCC.text())
        mccode_config.compilation[str(self.ui.edtCflags.conf_var)] = str(self.ui.edtCflags.text())
        mccode_config.compilation[str(self.ui.edtMpicc.conf_var)] = str(self.ui.edtMpicc.text())
        mccode_config.compilation[str(self.ui.edtMPIrun.conf_var)] = str(self.ui.edtMPIrun.text())
        mccode_config.compilation[str(self.ui.edtNumNodes.conf_var)] = str(self.ui.edtNumNodes.text())
    
    def accept(self):
        self.__pullValuesTo_mccode_config()
        
        # finally
        super(McConfigDialog, self).accept()
    
    def save(self):
        self.__pullValuesTo_mccode_config()
        McGuiUtils.saveUserConfig(mccode_config,mccode_config.configuration["MCCODE"],mccode_config.configuration["MCCODE_VERSION"])
        
        # finally
        super(McConfigDialog, self).accept()
        
