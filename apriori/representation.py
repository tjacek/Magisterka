# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 16:02:50 2013

@author: tjacek
"""
import numpy as np
import mlpy as ml
import matplotlib.pyplot as plt

path="C:/Users/tjacek/Desktop/Magisterka/Magisterka/ApriorInput/mine_data"

def readDataset(filename,separator=","):
    dataset=[]
    for line in open(filename,'r'):
        line=line.replace('\n',"")
        line=line.replace(r"\s","")
        transaction=line.split(separator)
        dataset.append(transaction)
    return dataset
    
class BOW():
    def __init__(self):
        self.bowDict={}
    
    def add(self,item):
        if(self.contain(item)):
            self.bowDict[item]=self.size()
    
    def contain(self,item):
        return self.bowDict.get(item)==None
    
    def size(self):
        return len(self.bowDict.keys())
    
    def toVector(self,transaction):
        vector=np.zeros(self.size())
        for item in transaction:
            index=self.bowDict[item]
            vector[index]=1.0
        return vector

    def addAll(self,dataset):
        for transaction in dataset:
            for item in transaction:
                self.add(item)
    
def toVectors(dataset):
    bowRep=BOW()
    bowRep.addAll(dataset)
    vectors=[]
    for transaction in dataset:
        vectors.append(bowRep.toVector(transaction))
    return vectors

def dimReduction(x,k=2):
    pca=ml.PCA()
    pca.learn(x)
    return pca.transform(x, k)

def visualization(x):
    fig = plt.figure(1)
    plot1 = plt.scatter(x[:,0],x[:,1],alpha=0.75)
    plt.show()

def test():
    dataset=readDataset(path)
    vectors=toVectors(dataset)
    rep2D=dimReduction(vectors)
    visualization(rep2D)
	
test()