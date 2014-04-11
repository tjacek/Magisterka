# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""

import re

def saveArff(instances,path,filename):
    arff=toArff(instances)    
    myFile = open(path+filename, 'w')
    myFile.write(arff)
    myFile.close()

def toArff(points,labeled=True):
    arff=attrToText(points[0].size,labeled)
    arff+="@data\n"
    for point in points:
        arff+=str(point)    
    return arff +"\n"

def attrToText(n,labeled=True):
    s="@relation TestData \n \n"  
    for i in range(0,n):
        atrrName= "cord"+str(i)
        s+="@attribute "+ atrrName + " numeric\n"
    if(labeled):
        s+="@attribute cat {true,false}\n";
    return s +"\n"

def toStr(numeric):
    short=int(numeric*10)/10
    return str(short)

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
