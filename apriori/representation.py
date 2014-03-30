# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 16:02:50 2013

@author: tjacek
"""
import math,re

def convertDataset(filename,newfile):
    sep=r"( )+"
    newSep=","
    newDataset=""
    endline=r"(\s)*\n"
    for line in open(filename,'r'):
        line=re.sub(endline,r'\n',line)
        line=re.sub(sep,newSep,line)
        newDataset+=line
    f = open(newfile,"w")
    f.write(newDataset)
    f.close()
    return newDataset

def readDataset(filename,separator=","):
    dataset=[]
    for line in open(filename,'r'):
        line=line.replace('\n',"")
        line=line.replace(r"\s","")
        transaction=line.split(separator)
        dataset.append(transaction)
    return dataset

def avarage(numList):
    return sum(numList)/len(numList)
    
def std(numList):
    av=avarage(numList)
    sq=lambda x:(x-av)**2
    newList=list(map(sq,numList))
    var=avarage(newList)
    return math.sqrt(var)
    
def transactionStats(dataset):
    listSize=lambda x:float(len(x))
    numList=list(map(listSize,dataset))
    avg=avarage(numList)
    var=std(numList)
    return avg,var

#path="C:/Users/tjacek/Desktop/Magisterka/Magisterka/transactions/"

def getStats(path):
    repres={}
    dataset=readDataset(path)
    avg,var=transactionStats(dataset)
    repres["avg"]=avg
    repres["var"]=var
    return repres

def test():
    path="datasets/mine.data"
    rep=getStats(path)
    print(rep)

if __name__ == '__main__':
    test()
    
