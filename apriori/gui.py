import sys, os,bow,details
from PyQt4 import QtCore,QtGui

class MainWindow(QtGui.QMainWindow):

    def __init__(self, parent=None):
	QtGui.QMainWindow.__init__(self, parent)
	self.createMainWindow()

    def createMainWindow(self):
        self.constants()
        self.textFields={}
	self.setWindowTitle(self.title)
        self.resize(self.x,self.y)
        widget = self.initWidget()
        self.initLayout(widget)
        self.initList(widget)
        self.initInputs()
        self.initButtons()

    def constants(self):
        self.title='Apriori dataset generator'
        self.ends=".data"
        self.path="/home/user/Desktop/magisterka/apriori/datasets"
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

    def initList(self,widget):
        self.listWidget = QtGui.QListWidget(widget)
        x0=self.x
        y0=0.6*self.y
        self.listWidget.resize(x0,y0)
        self.layout.addWidget(self.listWidget);

    def initInputs(self):
        inputs = QtGui.QWidget()
        inputs.resize(300,300)
        formLayout=QtGui.QFormLayout()
        self.addField("Path",self.path,formLayout)
        self.addField("Output file","",formLayout)
        inputs.setLayout(formLayout)
        self.layout.addWidget(inputs)

    def initButtons(self):
	self.buttons = QtGui.QWidget()
        direction=QtGui.QBoxLayout.LeftToRight
        buttonsLayout = QtGui.QBoxLayout(direction)
        searchButton = self.addButton("search",self.searchButton)
        showButton   = self.addButton("stats",self.showButton)
        pcaButton   = self.addButton("pca",self.pcaButton)
        buttonsLayout.addWidget(searchButton)
        buttonsLayout.addWidget(showButton)
        buttonsLayout.addWidget(pcaButton)
        self.buttons.setLayout(buttonsLayout)
        self.layout.addWidget(self.buttons)

    def addButton(self,name,listener):
        button = QtGui.QPushButton(name, self)
        button.clicked.connect(listener)
        return button

    def addField(self,name,default,layout):
        text = QtGui.QTextEdit()
	text.append(default)
        text.setMaximumSize(400,40);
        self.textFields[name]=text
        layout.addRow(name,text)

    def getPath(self):
        field=self.textFields["Path"]
        path=field.toPlainText()
        return path

    def getCurrentDataset(self):
        return str(self.listWidget.currentItem().text())

    def showButton(self):
        dataset=self.getCurrentDataset() 
       # details.showDetails(dataset)
        w = details.DetailWindow(dataset,self)
        w.move(300, 300)
        w.show()

    def pcaButton(self):
        dataset=self.getCurrentDataset()
        bow.getPlot(dataset)

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
    w = MainWindow()
    w.move(300, 300)
    w.show()
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
