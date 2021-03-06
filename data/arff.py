# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 19:37:42 2013

@author: tjacek
"""

import re,ClassiferGenerator as gen

binaryCategories = ["true","false"]

def saveSplitedArff(dataset,path,filename,labeled=True,frac=0.3):
    data1,data2=dataset.split(frac)
    trainfile=filename.replace(".arff","Train.arff")
    saveArff(data1,path,trainfile,labeled)
    testfile=filename.replace(".arff","Test.arff")
    saveArff(data2,path,testfile,labeled)
    return trainfile,testfile

def saveArff(dataset,path,filename,labeled=True,categories=binaryCategories):
    arff=toArff(dataset,labeled,categories)    
    myFile = open(path+filename, 'w')
    myFile.write(arff)
    myFile.close()

def saveCsv(dataset,path,filename):
    csv=str(dataset)
    myFile = open(path+filename, 'w')
    myFile.write(csv)
    myFile.close()

def prepareOutput(testfile,outputfile,Categories=binaryCategories):
    f=open(outputfile,'r')
    output=f.read()
    f.close()
    labels=extractLabels(output)
    dataset=parseArff(testfile)[0]
    i=0
    for instance in dataset.instances:
        instance.setCategory(labels[i])
        i+=1
    arffOutput=toArff(dataset,True,Categories)
    f=open(outputfile, 'w')
    f.write(arffOutput)
    f.close()

def extractLabels(output):
    output=output.replace("[","")
    output=output.replace("].","")
    labels=output.split(",")
    pattern = re.compile(r'\s+')
    clean =lambda x: re.sub(pattern, '', x)
    return map(clean,labels)

def readArff(filename):
    file=open(filename,'r')
    arff=file.read()
    file.close()
    return arff

def toArff(dataset,labeled=True,categories=binaryCategories):
    arff=attrToText(dataset.dim,labeled,categories)
    arff+="@data\n"
    arff+=str(dataset)
    return arff +"\n"

def attrToText(n,labeled=True,categories=binaryCategories):
    s="@relation TestData \n \n"  
    for i in range(0,n):
        atrrName= "cord"+str(i)
        s+="@attribute "+ atrrName + " numeric\n"
    if(labeled):
        cats=""
        for cat in categories:
	    cats+=cat+","
        cats=cats[:-1]
        s+="@attribute cat {" + cats +"}\n";
    return s +"\n"

def parseArff(filename,unlabeled=False):
    arff=readArff(filename)
    attr,data=arff.split("@data\n")
    dim,attrNames=parseAttr(attr)
    if(unlabeled):
        points=parseUnlabeledPoints(data)
        dataset=gen.createUnlabeledDataset(points,dim)
    else:
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

def parseUnlabeledPoints(data):
    lines=data.split("\n")
    lines=filter(lambda l: len(l)>0.0,lines)
    lines=map(lambda x:x.split(","),lines)
    points=map(toFloat,lines)
    return points

def parsePoints(data):
    lines=data.split("\n")
    lines=filter(lambda l: len(l)>0.0,lines)
    lines=map(lambda x:x.split(","),lines)
    labels=map(lambda x:x.pop(-1),lines)
    points=map(toFloat,lines)
    #print(labels)
    #labels=map(toCat,labels)
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
    path="/home/user/Desktop/ML/data/dataOutput.arff"
    prepareOutput(path)
    #data,attr=parseArff("apriori/apriori.arff")
    #print(attr)
    
