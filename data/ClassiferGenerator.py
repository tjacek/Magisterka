# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 15:15:59 2013

@author: tjacek
"""
import math,random

def r(x,y):
    return math.sqrt(x*x + y*y)

def getCategory(x,y,z):
    if r(x,y)>z:
        return 1.0
    else:
        return 0.0
        
def uniform(scale=1.0,a=0.0,b=1.5):
    return scale * random.uniform(a,b)

def randomPoint(n=3,dist=uniform):
    point=[]
    for i in range(0,n):
        point.append(dist())
    return point

def randomSeq(n=10):
    rList=[]
    for i in range(0,n):
        rList.append(randomPoint())
    return rList
    
def classification(seq,fun):
    cseq=[]
    for point in seq:
        category=fun(point[0],point[1],point[2])
        point.append(category)
        cseq.append(point)
    return cseq

def createDataset(n):
    seq=randomSeq(n)
    return classification(seq,getCategory)

    