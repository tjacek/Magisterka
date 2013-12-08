# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 17:49:27 2013

@author: tjacek
"""
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

class Dataset(object):
    def __init__(self):
        self.X=[]
        self.Y=[]
        self.Z=[]
    
    def add(self,x,y,z):
        self.X.append(x)
        self.Y.append(y)
        self.Z.append(z)

    def size():
        return len(self.X)

    def toStr(self,i):
        return self.X[i] +"," + self.Y[i] +"," + self.Z[i]
    
    def __str__(self):
        s=""
        for i in range(0,len(self.X)):
            s+=self.toStr(i)+"\n"
        return s 

def toDataset(points):
    dataset=Dataset()
    for point in points:
        dataset.add(point[0],point[1],point[2])
    return dataset

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

def visualize(dataset):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(dataset.X,dataset.Y,dataset.Z,c='r',marker='o')
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    plt.show()
    
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
