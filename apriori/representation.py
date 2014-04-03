# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 16:02:50 2013

@author: tjacek
"""
import math,re
from sets import Set

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

def average(numList):
    return sum(numList)/len(numList)
    
def std(numList):
    av=average(numList)
    sq=lambda x:(x-av)**2
    newList=list(map(sq,numList))
    var=average(newList)
    return math.sqrt(var)
    
def transactionStats(dataset):
    listSize=lambda x:float(len(x))
    numList=list(map(listSize,dataset))
    avg=average(numList)
    var=std(numList)
    return avg,var

def itemsStats(items,dataset):
    freq=itemsFreq(items,dataset)
    avg=average(freq)
    var=std(freq)
    return avg,var

def itemsFreq(items,dataset):
    freq=[]
    for item in list(items):
        counter=0
        for transaction in dataset:
            if(item in Set(transaction)):
		counter+=1
        freq.append(counter)
    return freq

def getItems(dataset):
    items=Set([])
    for transaction in dataset:
	items=items.union(Set(transaction))
    return items

def getAttributes():
    return ["transactions","items","avg_transaction","var_transaction","avg_items","var_items"]

def getStats(path):
    stats={}
    dataset=readDataset(path)
    items=getItems(dataset)
    avgTrans,varTrans=transactionStats(dataset)
    avgItems,varItems=itemsStats(items,dataset)
    stats["transactions"]=len(dataset)
    stats["items"]=len(items)   
    stats["avg_transaction"]=avgTrans
    stats["var_transaction"]=varTrans
    stats["avg_items"]=avgItems
    stats["var_items"]=varItems
    return stats

def test():
    path="datasets/mine.data"
    rep=getStats(path)
    print(rep)

if __name__ == '__main__':
    test()
    
