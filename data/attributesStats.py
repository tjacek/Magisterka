import scipy.stats as stats,arff
import numpy as np

def getCorelationMatrix(filename):
    data,attr=arff.parseArff(filename)
    series=data.getAllDims()
    corelMatrix=computeCorelationMatrix(series)
    printCorlMatrix(corelMatrix,attr)
    return corelMatrix

def computeCorelationMatrix(series):
    size=len(series)-1
    corelMatrix = np.zeros(shape=(size,size))
    for i in range(0,size):
        for j in range(0,size):
            x=series[i]
            y=series[j]
            corl=stats.pearsonr(x,y)[0]
            corelMatrix[i][j]=round(corl,2)  
    return corelMatrix

def printCorlMatrix(corel,attr):
    size=len(corel)
    print(getFirstLine(size))
    for i in range(0,size):
        line=str(i)+": "
        for j in range(0,size):
            line+=getStr(corel[i][j])
        print(line)
    printAttributes(attr)

def getFirstLine(size):
    firstLine="   "
    for i in range(0,size):
        firstLine+=str(i)+"     "
    return firstLine

def printAttributes(attr):
    size=len(attr)
    for i in range(0,size):
        print(str(i) +" " + attr[i])

def getStr(corel):
   ws=" "
   if(corel==-0.0):
      corel=0.0
   if(corel>=0):
       ws+=" "
   if(int(corel*100)%10==0):
       ws+=" "
   return str(corel)+ws

class Histogram(object):
    def __init__(self,X,Y,bins=10):
        self.bins=bins
        self.matrix=np.zeros(shape=(bins,bins))
        self.getRange(X,Y)
        self.updateAll(X,Y)
        self.norm()

    def getRange(self,X,Y):
        self.minX=min(X)
        self.maxX=max(X)
        self.minY=min(Y)
        self.maxY=max(Y)
        self.detX=self.getDet(self.minX,self.maxX)
        self.detY=self.getDet(self.minY,self.maxY)

    def getDet(self,inf,sup):
        return (sup-inf)/self.bins

    def updateAll(self,X,Y):
        for i in range(0,len(X)):
            self.update(X[i],Y[i])

    def update(self,x,y):
        i,j=self.findBin(x,y)
        self.matrix[i][j]+=1.0

    def p(self,x,y): 
        i,j=self.findBin(x,y)
        return self.matrix[i][j]

    def findBin(self,x,y):
        for i in range(0,self.bins):
            if(self.xRange(x,i)):
		break
        for j in range(0,self.bins):
            if(self.yRange(y,j)):
		break
	return i,j
 
    def xRange(self,x,i):
        l=self.minX + i*self.detX
        u=l+self.detX 
        return (l<=x) and (x<=u) 

    def yRange(self,y,i):
        l=self.minY + i*self.detY
        u=l+self.detY 
        return (l<=y) and (y<=u)

    def norm(self):
        f=lambda x: sum(x)
        C=sum(map(f,self.matrix))
        div=lambda x: (x/C)
        self.matrix=map(lambda y:map(div,y),self.matrix)
        return C
    
    def __str__(self):
        return str(self.matrix)

def test():
    X=[1.0, 1.0, -1.0,-1.0]
    Y=[1.0,-1.0,  1,0,-1.0]
    H=Histogram(X,Y,2)
    #H.norm()
    print(H)

if __name__ == '__main__':
    #data=getCorelationMatrix("apriori/apriori.arff")
    test()
