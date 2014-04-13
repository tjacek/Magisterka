# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""

import re,ClassiferGenerator as gen

def saveArff(dataset,path,filename):
    arff=toArff(dataset)    
    myFile = open(path+filename, 'w')
    myFile.write(arff)
    myFile.close()

def readArff(filename):
    file=open(filename,'r')
    arff=file.read()
    file.close()
    return arff

def toArff(dataset,labeled=True):
    arff=attrToText(dataset.dim,labeled)
    arff+="@data\n"
    arff+=str(dataset)
    return arff +"\n"

def attrToText(n,labeled=True):
    s="@relation TestData \n \n"  
    for i in range(0,n):
        atrrName= "cord"+str(i)
        s+="@attribute "+ atrrName + " numeric\n"
    if(labeled):
        s+="@attribute cat {true,false}\n";
    return s +"\n"

def parseArff(filename):
    arff=readArff(filename)
    attr,data=arff.split("@data\n")
    dim=getDim(attr)
    points,labels=parsePoints(data)
    return gen.createNewDataset(points,labels)

def getDim(attr):
    lines=attr.split("\n")
    reg = re.compile('@attribute(.)+numeric(.)*')
    dim=0
    for line in lines:
	if(reg.match(line)):
	    dim+=1
    return dim

def parsePoints(data):
    lines=data.split("\n")
    lines=filter(lambda l: len(l)>0.0,lines)
    lines=map(lambda x:x.split(","),lines)
    labels=map(lambda x:x.pop(-1),lines)
    points=map(toFloat,lines)
    labels=map(toCat,labels)
    return points,labels

def toFloat(rawPoint):
    return map(float,rawPoint)

def toCat(raw):
    p = re.compile(r"true|false")
    if(p.match(raw)):
       if(raw=="true"):
  	   return 1.0
       return -1.0     
    return 0.0

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
             
