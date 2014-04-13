# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 17:49:27 2013

@author: tjacek
"""
import matplotlib.pyplot as plt,ClassiferGenerator as gen
from mpl_toolkits.mplot3d import Axes3D

colors=['r','b','g']

def visualizeLabels(dataset):
    dataSeries=dataset.separate()
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
print(dataset)
#prepData=prepare(dataset)
visualizeLabels(dataset)


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
    
def visualizePoints(points):
    pos,neg =separatePoints(points)
    pos=toDataset(pos)
    neg=toDataset(neg)
    visualizeLabels(pos,neg)
