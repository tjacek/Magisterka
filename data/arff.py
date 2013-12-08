# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""
import re
 
def genAtr(labeled):
    atrributes={}
    atrributes["x"]="numeric"
    atrributes["y"]="numeric"
    atrributes["z"]="numeric"
    if(labeled):    
        atrributes["golf"]="{true,false}"
    return atrributes

def attrToText(labeled):
    s="@relation TestData \n \n"
    attr=genAtr(labeled)    
    for key in list(attr.keys()):
        s+="@attribute "+ key + " " + attr[key] +"\n"
    return s +"\n"
    
def getLabel(x):
    s= str(bool(x))
    return s.lower()

def toStr(numeric):
    short=int(numeric*10)/10
    return str(short)

def pointToStr(p,labeled):
    s=toStr(p[0]) +"," + toStr(p[1]) + ","+ toStr(p[2])
    if(labeled):
        s+="," + getLabel(p[3])
    return s

def toArff(points,labeled):
    arff=attrToText(labeled)
    arff+="@data\n"
    for point in points:
        arff+=pointToStr(point,labeled) +"\n"    
    return arff +"\n"

def saveArff(points,labeled=True,file="Test3.arff",path="C:/Users/tjacek/IdeaProjects/ML/data/" ):
    arff=toArff(points,labeled)    
    myFile = open(path+file, 'w')
    myFile.write(arff)
    myFile.close()

def toFloat(raw):
    point=[]
    for s in raw:
        point.append(float(s))
    return point
    
def parseData(filename):
    p = re.compile('(\S)+,(\S)+,(\S)+')
    points=[]
    for line in open(filename,'r').readlines():
        if(p.match(line)):
            point=line.split(",")
            points.append(toFloat(point))
    return points

def toLabel(label):
    if(label=="true"):
        return 1.0
    return 0.0
            
def parseLabels(filename):
    file=open(filename,'r')
    rawLabels=file.read()
    file.close()
    rawLabels=rawLabels.replace("[","").replace("].","")
    labels=[]    
    for label in rawLabels.split(","):
        labels.append(toLabel(label))
    return labels
    
def parseArff(dataFile,labelsFile):
    data=parseData(dataFile)
    labels=parseLabels(labelsFile)
    i=0
    points=[]
    for point in data:
        point.append(labels[i])
        points.append(point)
        i+=1
    return points    

def createInput():
    points1=createDataset(20)
    saveArff(points1,labeled=True,file="Training2.arff")
    points2=createDataset(20)
    saveArff(points2,labeled=False,file="Input2.arff")

def showOutput():
    path="C:/Users/tjacek/IdeaProjects/ML/data/"
    data="Input2.arff"
    labels="Output2.txt"
    points=parseArff(path+data,path+labels)
    print(points)
    visualizePoints(points)    
    
def test():
    path="C:/Users/tjacek/IdeaProjects/ML/data/"
    data="NoLabels.arff"
    labels="Output"
    return parseArff(path+data,path+labels)
    