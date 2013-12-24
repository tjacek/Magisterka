# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""

import re

def attrToText(labeled):
    s="@relation TestData \n \n"
    attr=["x","y","z"]    
    for key in attr:
        s+="@attribute "+ key + " numeric\n"
    if(labeled):
        s+="@attribute cat {true,false}\n";
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

def saveArff(points,labeled=True,file="Test3.arff",path="C:/Users/tjacek/IdeaProjects/ML/" ):
    arff=toArff(points,labeled)    
    myFile = open(path+file, 'w')
    myFile.write(arff)
    myFile.close()

def toFloat(raw):
    p = re.compile(r"true|false")
    point=[]
    for s in raw:
        if(p.match(s)):
            point.append(toLabel(s))
        else:    
            point.append(float(s))
    return point

def parseLabeledData(filename):
    reg=r"(\S)+,(\S)+,(\S)+,(true|false)"
    p = re.compile(reg)
    points=[]
    for line in open(filename,'r').readlines():
        if(p.match(line)):
            point=line.split(",")
            points.append(toFloat(point))
    return points
    
def parseData(filename):
    p = re.compile('(\S)+,(\S)+,(\S)+')
    points=[]
    for line in open(filename,'r').readlines():
        if(p.match(line)):
            point=line.split(",")
            points.append(toFloat(point))
    return points

def toLabel(label):
    label=label.replace("\n","")
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
    
def showInput(filename):
    path="C:/Users/tjacek/IdeaProjects/ML/train/"
    points=parseLabeledData(path+filename)
    visualizePoints(points)    

def showOutput(points="test/nonlinear.arff",labels="result/nonlinearC45.arff"):
    path="C:/Users/tjacek/IdeaProjects/ML/"
    points=parseArff(path+points,path+labels)
    visualizePoints(points)      