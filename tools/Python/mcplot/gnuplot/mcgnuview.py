#!/usr/bin/env python
#
#  PyQt-based user interface for mcplot-gnuplot.
#
import sys
from PyQt4 import QtGui, QtCore
from mcgnuwidgets import Ui_McGnuWindow

def startGui(plotter):
    app = QtGui.QApplication(sys.argv)
    
    mediator = McGnuMediator(plotter)
    mediator.initUi()
    mediator.setupCallbacks()
    mediator.showUi()
    
    sys.exit(app.exec_())

class McGnuMediator():
    def __init__(self, plotter):
        self.__plotter = plotter
        self.__mcgv = McGnuView()

    def initUi(self):
        self.__mcgv.initUi(self.__plotter.get_data_keys())
    
    def setupCallbacks(self):
        self.__mcgv.ui.lstvMonitors.clicked.connect(self.itemMouseClick)

    def showUi(self):
        self.__mcgv.show()
        self.__plotter.plot()
        
    def itemMouseClick(self, idx):
        # idx: a QtCore.QModelIndex object that was just clicked
        if idx.row() == 0:
            self.__plotter.plot()
        else:
            key = str(idx.data().toPyObject())
            print(key)
            self.__plotter.plot_single(key)
        
class McGnuView(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(McGnuView, self).__init__(parent)
        self.ui = Ui_McGnuWindow()
        self.ui.setupUi(self)
    
    def initUi(self, keys):
        self.ui.lstvMonitors.addItem(QtGui.QListWidgetItem('<overview plot>'))
        for k in keys:
            self.ui.lstvMonitors.addItem(QtGui.QListWidgetItem(QtCore.QString(k)))
        
        