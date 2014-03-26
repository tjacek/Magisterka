# -*- coding: utf-8 -*-
"""
Created on Thu Dec 26 19:05:45 2013

@author: tjacek
"""
import numpy as np
import mlpy
import matplotlib.pyplot as plt
import representation as rep

class BOW():
    def __init__(self):
        self.bowDict={}
        self.items={}
    
    def add(self,item):
        if(not self.contain(item)):
            index=self.size()
            self.bowDict[item]=index
            self.items[index]=1
        else:
            index=self.bowDict[item]
            self.items[index]+=1
    
    def contain(self,item):
        return self.bowDict.get(item)!=None
    
    def size(self):
        return len(self.bowDict.keys())

    def getItems(self):
        itemList=[]
        for i in range(0,self.size()):
            itemList.append(self.items[i])
        return itemList
    
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

def createBow(dataset):
    bowRep=BOW()
    bowRep.addAll(dataset)
    return bowRep
    
def toVectors(dataset):
    bowRep=createBow(dataset)
    vectors=[]
    for transaction in dataset:
        vectors.append(bowRep.toVector(transaction))
    return vectors

def toMatrix(data):
    return np.asarray(data)

def dimReduction(vectors,k=2):
    x=toMatrix(vectors)
    pca=mlpy.PCA()
    pca.learn(x)
    return pca.transform(x, k)

def get2DBow(dataset):
    vectors=toVectors(dataset)
    return dimReduction(vectors)

def visualization(x):
    fig = plt.figure(1)
    plot = plt.scatter(x[:,0],x[:,1],alpha=0.75)
    plt.show()
    return plot
    
def itemStats(dataset):
    bowRep=createBow(dataset)
    numList=bowRep.getItems()
    avg=rep.avarage(numList)
    var=rep.std(numList)
    return avg,var    

def getPlot(path):
    dataset=rep.readDataset(path)
    vectors=get2DBow(dataset)
    return visualization(vectors)

def test(path="/home/user/Desktop/magisterka/transactions/mine_data"):
    dataset=rep.readDataset(path)
    vectors=get2DBow(dataset)
    print(vectors)
    visualization(vectors)

def test2(path="datasets/mine.data"):
    dataset=rep.readDataset(path)
    avg,var=itemStats(dataset)
    print(avg)
    print(var)

if __name__ == '__main__':
    test("datasets/mine.data")

