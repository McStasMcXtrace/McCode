#!/usr/bin/env python
#
#  PyQt-based user interface for mcplot-gnuplot.
#
import sys
from PyQt4 import QtGui, QtCore
from mcgnuwidgets import Ui_McGnuWindow

# setup and start the gnuplot app
def startGui(plotter):
    app = QtGui.QApplication(sys.argv)
    
    gnuview = McGnuView()
    app.installEventFilter(gnuview)
    
    mediator = McGnuMediator(plotter, gnuview)
    mediator.initUi()
    mediator.setupCallbacks()
    mediator.showUi()
    
    sys.exit(app.exec_())

# mediate references and events
class McGnuMediator():
    def __init__(self, plotter, gnuview):
        self.__plotter = plotter
        self.__mcgv = gnuview

    def initUi(self):
        self.__mcgv.initUi(self.__plotter.get_data_keys())

    def setupCallbacks(self):
        self.__mcgv.ui.lstvMonitors.clicked.connect(self.itemMouseClick)
        self.__mcgv.ui.btnCloseAll.clicked.connect(lambda: self.__plotter.closeAllGnuplots())
        self.__mcgv.ui.btnSaveAs.clicked.connect(lambda: self.__mcgv.ui.statusBar.showMessage('not implemented'))
        self.__mcgv.ui.cbxLogScale.stateChanged.connect(self.logScaleCommand)

    def logScaleCommand(self, checked_state):
        if checked_state == 0:
            self.__mcgv.ui.statusBar.showMessage('')
        elif checked_state == 2:
            self.__mcgv.ui.statusBar.showMessage('Log scale enabled')
        
    def showUi(self):
        self.__mcgv.show()
        self.__plotter.plot()
        
    # callback for list item mouse click
    def itemMouseClick(self, idx):
        # idx: a QtCore.QModelIndex object that was just clicked
        self.__mcgv.ui.statusBar.showMessage('Plotting: %s' % idx.data().toPyObject())
        if idx.row() == 0:
            self.__plotter.plot()
        else:
            key = str(idx.data().toPyObject())
            print(key)
            self.__plotter.plot_single(key)

# Widget wrapper class. Install as app-wide event filter to receive all keypress events.
class McGnuView(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(McGnuView, self).__init__(parent)
        self.ui = Ui_McGnuWindow()
        self.ui.setupUi(self)

    def initUi(self, keys):
        self.ui.lstvMonitors.addItem(QtGui.QListWidgetItem('< overview >'))
        for k in keys:
            self.ui.lstvMonitors.addItem(QtGui.QListWidgetItem(QtCore.QString(k)))

    # enables this class as an event filter
    def eventFilter(self,  obj,  event):
        if event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_L:
                self.ui.cbxLogScale.setChecked(not self.ui.cbxLogScale.isChecked())
            return True
        return False
