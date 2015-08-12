#!/usr/bin/env python
#
# PyQt-based user interface for mcplot-gnuplot.
#
import sys
from PyQt4 import QtCore, QtGui
from mcgnuwidgets import Ui_McGnuWindow
from mcgnuplotter import McGnuplotter

def startGui():
    app = QtGui.QApplication(sys.argv)
    mySW = McGnuView()
    mySW.show()
    sys.exit(app.exec_())


class McGnuMediator():
    __member = ''

class McGnuView(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(McGnuView, self).__init__(parent)
        self.ui = Ui_McGnuWindow()
        self.ui.setupUi(self)

