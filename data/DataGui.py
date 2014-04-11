import sys, os
from PyQt4 import QtCore,QtGui

class MainWindow(QtGui.QMainWindow):

    def __init__(self, parent=None):
	QtGui.QMainWindow.__init__(self, parent)
        self.init()
	
    def init(self):
        self.createMainWindow()
        widget = self.initWidget()
        self.initLayout(widget)
        self.initList(widget)
        self.initInputs()
        self.initButtons()
    
    def createMainWindow(self):
        self.constants()
        self.textFields={}
        self.button={}
	self.setWindowTitle(self.title)
        self.resize(self.x,self.y)
        
    def constants(self):
        self.title='Arff dataset generator'
        self.ends=".arff"
        self.path="/home/user/Desktop/ML/data"
	self.margin=5.0
        self.x=500.0
        self.y=500.0

    def initWidget(self):
        widget = QtGui.QWidget(self)
        widget.setGeometry(QtCore.QRect(0, 0, self.x, self.y))
	return widget 

    def initLayout(self,widget):
        direction=QtGui.QBoxLayout.TopToBottom
        self.layout=QtGui.QBoxLayout(direction)
        widget.setLayout(self.layout)

    def initList(self,widget):
        self.listWidget = QtGui.QListWidget(widget)
        x0=self.x
        y0=0.6*self.y
        self.listWidget.resize(x0,y0)
        self.layout.addWidget(self.listWidget);

    def initButtons(self):
        return None

    def initInputs(self):
	return None

    def addButton(self,name,listener):
        button = QtGui.QPushButton(name, self)
        button.clicked.connect(listener)
        self.button[name]=button
        return button

    def addField(self,name,default,layout):
        text = QtGui.QTextEdit()
	text.append(default)
        text.setMaximumSize(400,40);
        self.textFields[name]=text
        layout.addRow(name,text)

class ArffWindow(MainWindow):

    def __init__(self, parent=None):
	MainWindow.__init__(self, parent)

    def initButtons(self):
	self.buttons = QtGui.QWidget()
        direction=QtGui.QBoxLayout.LeftToRight
        buttonsLayout = QtGui.QBoxLayout(direction)
        searchButton = self.addButton("search",self.searchButton)
        showButton = self.addButton("show",self.showButton)
        createButton = self.addButton("create",self.createButton)
        runButton = self.addButton("run",self.runButton)
        buttonsLayout.addWidget(searchButton)
        buttonsLayout.addWidget(showButton)
        buttonsLayout.addWidget(createButton)
        buttonsLayout.addWidget(runButton)
        self.buttons.setLayout(buttonsLayout)
        self.layout.addWidget(self.buttons)

    def initInputs(self):
        inputs = QtGui.QWidget()
        inputs.resize(300,400)
        formLayout=QtGui.QFormLayout()
        self.addField("Path",self.path,formLayout)
        self.addField("Filename","",formLayout)
        self.addField("Numbers of point","100.0",formLayout)
        self.addField("Numbers of dim","2.0",formLayout)
        inputs.setLayout(formLayout)
        self.layout.addWidget(inputs)

    def showButton(self):
	return None

    def createButton(self):
        return None

    def runButton(self):
        return None

    def searchButton(self):
	self.listWidget.clear()
	sender = self.sender()
	for root, dirs, files in os.walk(self.path):
	    for file in files:
		if file.endswith(self.ends):
		    try:
			path = os.path.join(root, file).encode('utf-8')
			self.listWidget.addItem(path)
		    except:
			pass

def main():
    app = QtGui.QApplication(sys.argv)
    w = ArffWindow()
    w.move(300, 300)
    w.show()
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
