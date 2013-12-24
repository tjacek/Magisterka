# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 15:15:59 2013

@author: tjacek
"""
import math,random

def r(x,y):
    return math.sqrt(x*x + y*y)
        
def uniform(scale=1.0,a=0.0,b=1.0):
    return scale * random.uniform(a,b)

def getUniformDist(scale,a=0.0,b=1.0):
    return lambda : uniform(scale,a,b)

def randomPoint(dist,k=3):
    point=[]
    for i in range(0,k):
        point.append(dist())
    return point

def randomSeq(k=10,scale=1.0):
    rList=[]
    dist=getUniformDist(scale)
    for i in range(0,k):
        rList.append(randomPoint(dist))
    return rList
    
def classification(seq,pred):
    cseq=[]
    for point in seq:
        category=pred(point[0],point[1],point[2])
        point.append(category)
        cseq.append(point)
    return cseq

def createDataset(n,pred,scale=1.0):
    seq=randomSeq(n,scale)
    return classification(seq,pred)

def getPositives(dataset):
    categories=list(map(lambda p:p[3], dataset))
    return sum(categories)/len(categories)    
        