# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 17:49:27 2013

@author: tjacek
"""
import matplotlib.pyplot as plt,ClassiferGenerator as gen
from mpl_toolkits.mplot3d import Axes3D

def prepare(dataset):
    dataSeries=[]
    for i in range(0,3):
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

dataset=gen.generateDataset(20,3,gen.linearPredict)
prepData=prepare(dataset)
visualize(prepData)


def separate(points,predict):
    pos=[]
    neg=[]
    for point in points:
        if(predict(point)):
            pos.append(point)
        else:
            neg.append(point)
    return pos,neg    

def separatePoints(points):
    predict =lambda point: point[3]==1.0
    return separate(points,predict)
    
def visualizeLabels(pos,neg):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(pos.X,pos.Y,pos.Z,c='r',marker='o')
    ax.scatter(neg.X,neg.Y,neg.Z,c='b',marker='o')
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    plt.show()
    
def visualizePoints(points):
    pos,neg =separatePoints(points)
    pos=toDataset(pos)
    neg=toDataset(neg)
    visualizeLabels(pos,neg)
