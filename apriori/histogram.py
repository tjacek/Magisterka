import numpy as np,math

class Histogram(object):
    def __init__(self,vector2D,dim=10.0):
	self.dim=float(dim)
        self.matrix=np.zeros(shape=(dim,dim))
        self.max=max(getMax(vector2D))
        self.min=min(getMin(vector2D))
        self.size=math.fabs(self.min -self.max)
        self.h=self.size/self.dim
        self.createHist(vector2D)
        self.u=self.avg()

    def createHist(self,vector2D):
        for point in vector2D:
            ds=self.convertPoint(point)
            self.matrix[ds[0]][ds[1]]=self.matrix[ds[0]][ds[1]]+1.0

    def convertPoint(self,point):
        x=(point[0]-self.min)/self.size
        y=(point[1]-self.min)/self.size
        x= int(10.0*x)
        y= int(10.0*y)
        if(x>=self.dim):
           x-=1
        if(y>=self.dim):
           y-=1
        return x,y

    def avg(self):
        value=0.0
        for row in self.matrix:
	    for b in row:
		value+=b
        return value/(self.dim*self.dim)

    def maxValue(self):
        value=self.matrix[0][0]
        for row in self.matrix:
	    for b in row:
		if(value<b):
		    value=b
        return value

    def minValue(self):
        value=self.matrix[0][0]
        for row in self.matrix:
	    for b in row:
		if(value>b):
		    value=b
        return value

    def mediana(self):
	allBins=self.matrix.flatten()
        allBins.sort()
        allBins=filter(lambda x: x != 0.0, allBins)
        print(allBins)
        center=float(len(allBins))
        if(center % 2 ==0):
           return allBins[center/2]
        else:
           med= int(center /2)
           a=allBins[med]
           b=allBins[med+1]
           return (a+b)/2.0

    def var(self):
        value=0.0
        for row in self.matrix:
	    for x in row:
		v=(x-self.u)
                value+=v*v
        value/=self.dim*self.dim -1.0 
        return math.sqrt(value)	

def getMax(vector2D):
    point=vector2D[0]
    maxXY=[point[0],point[1]]
    for point in vector2D:
	if(maxXY[0]<point[0]):
            maxXY[0]=point[0]
        if(maxXY[1]<point[1]):
            maxXY[1]=point[1]
    return (maxXY[0],maxXY[1])

def getMin(vector2D):
    point=vector2D[0]
    minXY=[point[0],point[1]]
    for point in vector2D:
	if(minXY[0]>point[0]):
            minXY[0]=point[0]
        if(minXY[1]>point[1]):
            minXY[1]=point[1]
    return (minXY[0],minXY[1])
