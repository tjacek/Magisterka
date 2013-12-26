# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 19:05:45 2013

@author: tjacek
"""
import numpy as np
import mlpy as ml
import matplotlib.pyplot as plt
import representation

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

def toMatrix(data):
    return np.asarray(data)

def dimReduction(vectors,k=2):
    x=toMatrix(vectors)
    pca=ml.PCA()
    pca.learn(x)
    return pca.transform(x, k)

def get2DBow(dataset):
    vectors=toVectors(dataset)
    return dimReduction(vectors)

def visualization(x):
    fig = plt.figure(1)
    plot1 = plt.scatter(x[:,0],x[:,1],alpha=0.75)
    plt.show()

def test(path="/home/user/Desktop/magisterka/transactions/mine_data"):
    dataset=readDataset(path)
    vectors=get2DBow(dataset)
    print(vectors)
    visualization(vectors)