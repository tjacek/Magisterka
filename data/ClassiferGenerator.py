# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 15:15:59 2013

@author: tjacek
"""
import math,random

class Instance(object):
    def __init__(self,point,category):
	self.point=point
	self.category=category

    def __str__(self):
        s=str(self.point)+"\n"
        s+=str(self.category)
        return s

def generateDataset(n,dim,pred,scale=10):
    seq=randomSeq(n,dim,scale)
    return classification(seq,pred)

def randomSeq(n=10,dim=3,scale=1.0):
    rList=[]
    dist=getUniformDist(scale)
    for i in range(0,n):
        rList.append(randomPoint(dist,dim))
    return rList

def randomPoint(dist,dim=3):
    point=[]
    for i in range(0,dim):
        point.append(dist())
    return point

def r(x,y):
    return math.sqrt(x*x + y*y)
        
def uniform(scale=1.0,a=0.0,b=1.0):
    return scale * random.uniform(a,b)

def getUniformDist(scale,a=0.0,b=1.0):
    return lambda : uniform(scale,a,b)
    
def classification(seq,pred):
    cseq=[]
    for point in seq:
        category=pred(point)
        instance=Instance(point,category)
        cseq.append(instance)
    return cseq

def linearPredict(point):
    if(sum(point)>5.0):
        return 1.0
    else:
        return 0.0

def nonLinearPredict(point):
    sqr=list(map(lambda x:x*x,point))
    sqr=sum(sqr)
    if(sqr>10.0):
        return 1.0
    else:
        return 0.0

predDir ={ "linear":linearPredict,"nonLinear":nonLinearPredict}
#def getPositives(dataset):
#    categories=list(map(lambda p:p[3], dataset))
#    return sum(categories)/len(categories)    
        
