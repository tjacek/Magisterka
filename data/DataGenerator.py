# -*- coding: utf-8 -*-
"""
Created on Tue Oct  1 13:25:26 2013

@author: tjacek
"""
import numpy
from numpy import random
from pylab import scatter,show

class DataGenerator(object):
    def __init__(self,x=uniform,y=numpy.sin,noise=zero):
        self.x=x
        self.y=y
        self.noise=noise
        
    def generateX(self,n):
        X=[]
        for i in range(1,n):
            X.append(self.x())
        return X
            
    def generateY(self,X):
        Y=[]
        for x in X:
            y=self.y(x) + self.noise()
            Y.append(y)
        return Y
            
    def generate(self,n):
        X = self.generateX(n)
        Y = self.generateY(X)
        return [X,Y]

def zero():
    return 0
    
def uniform():
    scale=1.0
    return scale * random.uniform()

def visualise(data):
    x=data[0]
    y=data[1]
    scatter(x,y)
    show()
