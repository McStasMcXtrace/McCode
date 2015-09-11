#!/usr/bin/env python
#
#  PyQt-based user interface for mcplot-gnuplot.
#
import sys
from PyQt4 import QtGui, QtCore
from mcgnuwidgets import Ui_McGnuWindow

# setup and start the gnuplot app
def startGui(plotter, log_scale=False):
    app = QtGui.QApplication(sys.argv)
    
    gnuview = McGnuView()
    app.installEventFilter(gnuview)
    
    mediator = McGnuMediator(plotter, gnuview)
    mediator.initUi(log_scale)
    mediator.setupCallbacks()
    mediator.showUi()
    
    sys.exit(app.exec_())

# mediate references and events
class McGnuMediator():
    def __init__(self, plotter, gnuview):
        self.__plotter = plotter
        self.__mcgv = gnuview

    # must be called before setupCallbacks
    def initUi(self, log_scale):
        self.__mcgv.initUi(self.__plotter.getDataKeys(), log_scale)

    # strictly limit widget access by McGnuMediator to this method
    def setupCallbacks(self):
        self.__mcgv.ui.lstvMonitors.clicked.connect(self.itemMouseClick)
        self.__mcgv.ui.btnCloseAll.clicked.connect(self.handleCloseAll)
        self.__mcgv.ui.btnSave.clicked.connect(self.handleSave)
        self.__mcgv.ui.cbxLogScale.stateChanged.connect(self.setLogscale)

    def handleSave(self):
        key = self.__mcgv.getSelectedKey()
        self.__plotter.save(key)
        self.__mcgv.showMessage('Saved: %s' % key)

    def handleCloseAll(self):
        self.__plotter.closeAll()
        self.__plotter = self.__plotter.getSimilarInstance()
        self.__plotter.setLogscale(self.__mcgv.isLogScaleEnabled())

    def setLogscale(self, checked_state):
        if checked_state == 0:
            self.__plotter.setLogscale(False)
            self.__mcgv.showMessage('')
        elif checked_state == 2:
            self.__plotter.setLogscale(True)
            self.__mcgv.showMessage('Log scale enabled')

    def showUi(self):
        self.__mcgv.show()
        self.__plotter.plot(self.__plotter.getDataKeys()[0])

    # callback for list item mouse click
    def itemMouseClick(self, idx):
        # idx: a QtCore.QModelIndex object that was just clicked
        key = str(idx.data().toPyObject())
        self.__mcgv.showMessage('Plotting: %s' % key)
        self.__plotter.plot(key)

# Widget wrapper class. Install as app-wide event filter to receive all keypress events.
class McGnuView(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(McGnuView, self).__init__(parent)
        self.ui = Ui_McGnuWindow()
        self.ui.setupUi(self)
    
    def initUi(self, keys, log_scale):
        for k in keys:
            self.ui.lstvMonitors.addItem(QtGui.QListWidgetItem(QtCore.QString(k)))
        self.ui.cbxLogScale.setChecked(log_scale)
    
    def showMessage(self, msg):
        self.ui.statusBar.showMessage(msg)
    
    def isLogScaleEnabled(self):
        return self.ui.cbxLogScale.isChecked()
    
    def getSelectedKey(self):
        selected_items = self.ui.lstvMonitors.selectedItems()
        if len(selected_items)==0:
            return str(self.ui.lstvMonitors.item(0).text())
        return str(self.ui.lstvMonitors.selectedItems()[0].text())
    
    # enables this class as an event filter
    def eventFilter(self,  obj,  event):
        if event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_L:
                self.ui.cbxLogScale.setChecked(not self.ui.cbxLogScale.isChecked())
            if event.key() == QtCore.Qt.Key_Q:
                self.close()
            return True
        return False
