import os,time

def experiment(datasets,bounds):
    results={}
    i=0
    for aprioriDataset in datasets:
        print("Dataset " + str(i) +"\n")
        expsForDataset=[]
        for params in getParams(bounds,aprioriDataset):
 	    time=execute(params.minSup,params.minConf,params.workers,aprioriDataset)
            params.time=time
            expsForDataset.append(params)
        results[aprioriDataset]=expsForDataset
        i+=1
    return results

def getParams(bounds,name):
    paramsList=[]
    for minSup in getRange(bounds.lowerSup,bounds.upperSup):
	for minConf in getRange(bounds.lowerConf,bounds.upperConf):
            for workers in getRange(bounds.lowerWorkers,bounds.upperWorkers,1.0):
                param= AprioriParametrs(name,minSup,minConf,int(workers))
		paramsList.append(param)
    return paramsList

def getRange(a,b,h=0.1):
    det=b-a
    size=int(det/h)
    interval=[]
    for i in range(0,size):
	interval.append(a+i*h)
    return interval

class Bounds(object):
    def __init__(self,lowerSup,upperSup,lowerConf,upperConf,lowerWorkers,upperWorkers):
        self.lowerSup=float(lowerSup)
        self.upperSup=float(upperSup)
        self.lowerConf=float(lowerConf)
        self.upperConf=float(upperConf)
        self.lowerWorkers=int(lowerWorkers)
        self.upperWorkers=int(upperWorkers)

class AprioriParametrs(object):
    def __init__(self,name,minSup,minConf,workers,time=0.0):
	self.name=name
        self.minSup=minSup
	self.minConf=minConf
	self.workers=workers
	self.time=time

    def __str__(self):
        s=str(self.minSup)+","
        s+=str(self.minConf)+","
        s+=str(self.workers)+","
        s+=str(self.time)+"\n"
        return s

def execute(sup,conf,work,data,path="/home/user/Desktop/ML/src"): 
    cmd ="erl -pa " + path +" -run apriori extCall " 
    MinSup=str(sup)
    MinConf=str(conf)
    Workers=str(work) 
    Dataset=data 
    print("MinConf"+ MinConf+ "MinSup" + MinSup +" Workers "+ Workers +" " + Dataset  +"\n")
    postfix=" -run init stop -noshell"
    cmd+=MinSup + " " + MinConf +" " +Workers+" "
    cmd+=Dataset + postfix
    time1=getTime()
    os.system(cmd)
    time2=getTime()
    time =time2-time1
    return time#AprioriParametrs(MinSup,MinConf,Workers,time)

def getTime():
    return int(round(time.time() * 1000))

if __name__ == '__main__':
  
    #defbounds=Bounds(0.3,0.7,0.3,0.7,1,2)
    #experiment(datasets,defbounds)
    path1="datasets/old/mine.data"
    path2="transactions/dataset1/trans11.data"
    print(execute(0.5,0.5,1,path1))
