# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 17:49:27 2013

@author: tjacek
"""
import numpy as np,mlpy,matplotlib.pyplot as plt,ClassiferGenerator as gen
from mpl_toolkits.mplot3d import Axes3D

colors=['r','b','g']

def visualizeLabels(dataset):
    if(dataset.dim>3):
	dataset=dimReduction(dataset)
    dataSeries=dataset.separate()
    if(dataset.dim==2):
        visualizeLabels2D(dataSeries)
        return
    visualizeLabels3D(dataSeries)

def visualizeLabels3D(dataSeries):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    i=0
    for key in dataSeries.keys():
       prepData=prepare(dataSeries[key])
       ax.scatter(prepData[0],prepData[1],prepData[2],c=colors[i],marker='o')
       i+=1 
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    plt.show()

def visualizeLabels2D(dataSeries):
    fig = plt.figure()
    ax = fig.add_subplot(111)#, projection='2d')
    i=0
    for key in dataSeries.keys():
       prepData=prepare(dataSeries[key])
       ax.scatter(prepData[0],prepData[1],c=colors[i],marker='o')
       i+=1 
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    plt.show()

def prepare(dataset):
    dataSeries=[]
    for i in range(0,dataset.dim):
        dataSeries.append(dataset.getDim(i))
    return dataSeries	

def visualize(preparedData):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(prepData[0],prepData[1],prepData[2],c='r',marker='o')
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    plt.show()

def dimReduction(dataset):
    points=dataset.getPoints()
    reduPoints=applyPCA(points)
    labels=dataset.getLabels()
    return gen.createNewDataset(reduPoints,labels)
     
def applyPCA(vectors):
    x=toMatrix(vectors)
    pca=mlpy.PCA()
    pca.learn(x)
    return pca.transform(x, 3)

def toMatrix(data):
    return np.asarray(data)
