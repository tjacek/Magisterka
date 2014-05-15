import numpy as np
import scipy.stats as stats,math
import matplotlib.pyplot as plt

def getCorl():
    x=getX(50)
    a,b=getY(x)
    return stats.pearsonr(a,b)[0]

def getY(x):
    a=map(lambda x:math.sin(x),x)
    b=map(lambda x:math.cos(x),x)
    return a,b

def getX(n):
    l=[]
    for i in range(0,n):
	l.append(i*0.3)
    return l

def visualize():
    ax = plt.figure()
   # ax = fig.add_subplot(111, projection='2d')
    x=getX(50)
    a,b=getY(x)
    fig = plt.figure(1)
    plot = plt.scatter(x , a,alpha=0.75,c='b')
    plot = plt.scatter(x , b,alpha=0.75,c='r')
    plt.show()

print(visualize())
