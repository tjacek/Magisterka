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

if __name__ == '__main__':
    data=getCorelationMatrix("apriori/apriori.arff")
