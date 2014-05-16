# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""

import re,ClassiferGenerator as gen

def saveSplitedArff(dataset,path,filename):
    data1,data2=dataset.split(0.3)
    trainfile=filename.replace(".arff","Train.arff")
    saveArff(data1,path,trainfile)
    testfile=filename.replace("Train.arff","Test.arff")
    saveArff(data2,path,testfile)

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
    dim,attrNames=parseAttr(attr)
    points,labels=parsePoints(data)
    dataset=gen.createNewDataset(points,labels,dim)
    return dataset,attrNames

def parseAttr(attr):
    lines=attr.split("\n")
    attrNames=[]
    reg = re.compile('@attribute(.)+numeric(.)*')
    dim=0
    for line in lines:
	if(reg.match(line)):
	    dim+=1
            name=extractAttrName(line)
            attrNames.append(name) 
    return dim,attrNames

def extractAttrName(line):
    line=line.replace("numeric","")
    return line.replace("@attribute","")

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

if __name__ == '__main__':
    data,attr=parseArff("apriori/apriori.arff")
    print(attr)
    
