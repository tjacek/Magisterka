import os,time

def experiment(datasets):
    results={}
    for aprioriDataset in datasets:
 	params=execute(0.5,0.5,1,aprioriDataset)
        results[aprioriDataset]=params
    return results

class AprioriParametrs(object):
    def __init__(self,minSup,minConf,workers,time):
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
    postfix=" -run init stop -noshell"
    cmd+=MinSup + " " + MinConf +" " +Workers+" "
    cmd+=Dataset + postfix
    time1=getTime()
    os.system(cmd)
    time2=getTime()
    time =time2-time1
    return AprioriParametrs(MinSup,MinConf,Workers,time)

def getTime():
    return int(round(time.time() * 1000))

if __name__ == '__main__':
    print(execute(0.5,0.5,1,"datasets/gen.data"))
