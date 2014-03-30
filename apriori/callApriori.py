import os,time

def experiment(datasets):
    results={}
    for aprioriDataset in datasets:
 	time=execute(0.5,0.5,1,aprioriDataset)
        results[aprioriDataset]=time
    return results

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
    return time2-time1

def getTime():
    return int(round(time.time() * 1000))
if __name__ == '__main__':
    print(execute(0.5,0.5,1,"datasets/gen.data"))
