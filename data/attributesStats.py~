import scipy.stats as stats
import numpy as np,arff

def corlMatrix(filename):
    corll,attr=getCorlMatrix(filename)
    return strMatrix(corll,attr)

def entropyMatrix(filename):
    entropy,attr=getEntropyMatrix(filename)
    return strMatrix(entropy,attr)

def getCorlMatrix(filename):
    return getMatrix(filename,computeCorelationMatrix)

def getEntropyMatrix(filename):
    return getMatrix(filename,mutualEntropyMatrix)

def getMatrix(filename,compute):
    data,attr=arff.parseArff(filename,True)
    series=data.getAllDims()
    corelMatrix=compute(series)
    return corelMatrix,attr

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

def printMatrix(corel,attr):
    rep=strMatrix(corel,attr)
    print(rep)

def strMatrix(corel,attr):
    size=len(corel)
    rep=getFirstLine(size)
    for i in range(0,size):
        line=str(i)+": "
        for j in range(0,size):
            value=round(corel[i][j],2) 
            line+=getStr(value)
        rep+= "\n"+line
    rep+=printAttributes(attr)
    return rep

def getFirstLine(size):
    firstLine="   "
    for i in range(0,size):
        firstLine+=str(i)+"     "
    return firstLine

def printAttributes(attr):
    value=""
    size=len(attr)
    for i in range(0,size):
        value+= "\n" + str(i) +" " + attr[i] 
    return value

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
        self.marginalize()
        self.norm()

    def getRange(self,X,Y):
        self.minX=min(X)
        self.maxX=max(X)
        self.minY=min(Y)
        self.maxY=max(Y)
        self.detX=self.getDet(self.minX,self.maxX)
        self.detY=self.getDet(self.minY,self.maxY)

    def getDet(self,inf,sup):
        return (sup-inf)/float(self.bins)

    def updateAll(self,X,Y):
        for i in range(0,len(X)):
            self.update(X[i],Y[i])

    def update(self,x,y):
        i,j=self.findBin(x,y)
        self.matrix[i][j]+=1.0

    def marginalize(self):
        self.px=map(lambda x:0.0,self.matrix)
        self.py=map(lambda x:0.0,self.matrix)
        for i in range(0,self.bins):
	    for j in range(0,self.bins):
                self.px[i]+=self.matrix[i][j]
        for i in range(0,self.bins):
	    for j in range(0,self.bins):
                self.py[i]+=self.matrix[j][i]        

    def p(self,x,y):
        if(x<self.minX or self.maxX<x):
            return 0.0 
        if(y<self.minY or self.maxY<y):
            return 0.0 
        i,j=self.findBin(x,y)
        return self.matrix[i][j]

    def margP(self,x,y):
        i,j=self.findBin(x,y)
        return self.px[i],self.py[j]

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
        self.px=map(div,self.px)
        self.py=map(div,self.py)
        return C

    def check(self):
        for i in range(0,self.bins):
            for j in range(0,self.bins):
                pxy=self.matrix[i][j]
                p2=self.px[i]*self.py[j]
                if(pxy!=0.0 and pxy <p2):
                    print(i,j,pxy,p2,self.px[i],self.py[i])

    def __str__(self):
        return str(self.matrix)

def mutualEntropyMatrix(series):
    size=len(series)
    entropyMatrix = np.zeros(shape=(size,size))
    for i in range(0,size):
        for j in range(0,size):
            x=series[i]
            y=series[j]   
            entropyMatrix[i][j]=mutualEntropy(x,y) 
    return entropyMatrix

def mutualEntropy(x,y):
    hist=Histogram(x,y,10)
    return entropy(hist.px) + entropy(hist.py) - jointEntropy(hist)

def entropy(pmarg):
    p=filter(nonzero,pmarg)
    fun=lambda x:x*np.log(1.0/x)
    return sum(map(fun,p))

def nonzero(x):
    return x>0.0

def jointEntropy(hist):
    entropy=0.0
    for i in range(0,hist.bins):
        for j in range(0,hist.bins):
	    entropy+=entropyDensity(i,j,hist)
    return entropy

def entropyDensity(i,j,hist):
    pxy=hist.matrix[i][j]
    if(pxy==0.0):
        return 0.0
    return  pxy*np.log(1.0/pxy)

