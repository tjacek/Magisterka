# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 16:02:50 2013

@author: tjacek
"""
import math

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

path="C:/Users/tjacek/Desktop/Magisterka/Magisterka/transactions/mine_data"

def test(file):
    dataset=readDataset(file)
    avg,var=transactionStats(dataset)
    print(avg)
    print(var)
    