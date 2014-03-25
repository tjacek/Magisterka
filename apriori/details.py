import sys, os
from PyQt4 import QtCore,QtGui

class DetailWindow(QtGui.QMainWindow):
    def __init__(self, parent=None):
        QtGui.QMainWindow.__init__(self, parent)
	self.createMainWindow()

    def createMainWindow(self):
        self.constants()
	widget = self.initWidget()
        self.initLayout(widget)
        self.initNumeric()
	return 0.0

    def constants(self):
        self.title='Apriori dataset generator'
        self.ends=".data"
        self.path="/home/user/Desktop/Gui/datasets"
	self.margin=5.0
        self.x=500.0
        self.y=300.0

    def initWidget(self):
        widget = QtGui.QWidget(self)
        widget.setGeometry(QtCore.QRect(0, 0, self.x, self.y))
	return widget

    def initLayout(self,widget):
        direction=QtGui.QBoxLayout.TopToBottom
        self.layout=QtGui.QBoxLayout(direction)
        widget.setLayout(self.layout)

    def initNumeric(self):
        numerics = QtGui.QWidget()
        formLayout=QtGui.QFormLayout()
        numerics.setLayout(formLayout)
        self.layout.addWidget(numerics)
        self.addField("Test",0.0,formLayout)

    def addField(self,name,value,layout):
        text = QtGui.QTextEdit()
        layout.addRow(name,text)

def main():
    app = QtGui.QApplication(sys.argv)
    w = DetailWindow()
    w.move(300, 300)
    w.show()
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
