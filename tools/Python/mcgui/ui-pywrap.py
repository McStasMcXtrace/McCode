
import sys
from PyQt4 import QtCore, QtGui

'''

INSERT auto-generated code here
CHANGE/DELETE PySide to PyQt4

e.g. using:

pyside-uic main.ui -o main-ui.py

'''

class ControlMainWindow(QtGui.QMainWindow):
  def __init__(self, parent=None):
    super(ControlMainWindow, self).__init__(parent)
    self.ui =  Ui_MainWindow()
    self.ui.setupUi(self)
   
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlMainWindow()
    mySW.show()
    sys.exit(app.exec_())
