import sys, os,representation as rep
from PyQt4 import QtCore,QtGui

class DetailWindow(QtGui.QMainWindow):
    def __init__(self, data,parent=None):
        QtGui.QMainWindow.__init__(self, parent)
        self.path=data
	self.createMainWindow()

    def createMainWindow(self):
        self.constants()
        self.resize(self.x,self.y)
	widget = self.initWidget()
        self.initLayout(widget)
        self.initNumeric()

    def constants(self):
        self.title='Apriori dataset generator'
        self.ends=".data"
        #self.path="datasets/mine.data"
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
        self.addAll(formLayout)

    def addAll(self,layout):
        dataset= rep.getStats(self.path)
        for name in dataset.keys():
            value=dataset[name]
            self.addField(name,value,layout)

    def addField(self,name,value,layout):
        text = QtGui.QTextEdit()
        text.setMaximumSize(400,40);
	text.append(str(value))
        layout.addRow(name,text)

def showDetails(data="datasets/mine.data"):
    app = QtGui.QApplication(sys.argv)
    w = DetailWindow(data)
    w.move(300, 300)
    w.show()
    sys.exit(app.exec_())

if __name__ == '__main__':
    showDetails()

