import sys,os,time
import arffConverter as arff,callApriori as call

def createDataset(filename,dirName):
    items=getFilenames(dirName)
    results=experiments(items)     
    arff.saveArffFile(results,None,filename)

def getFilenames(dirName):
    items = [ f for f in os.listdir(dirName)]
    currDir=os.getcwd()
    items=map(lambda x: currDir+"/"+dirName+"/"+x,items)
    return filter(isTransaction,items)

def isTransaction(filename):
    return filename.split(".")[1]=="data"

def experiments(datasets):
    results={}
    i=0
    for datasetName in datasets:
        print("Dataset " + str(i) +"\n")
        expsForDataset=[]
        minSup=[0.1,0.4,0.8]
        minConf=[0.2,0.4,0.7]
        items=[1]#,2,4]
        dataset_workers=[1]#,2,4]
        args=getArguments(datasetName,minSup,minConf,items,dataset_workers)
        for params in args:
 	    time=execute(params,datasetName)
            params.time=time
            expsForDataset.append(params)
        results[datasetName]=expsForDataset
        i+=1
    return results

def getArguments(name,minSupValues,minConfValues,workers1,workers2):
    paramsList=[]
    for minSup in minSupValues:
	for minConf in minConfValues:
            for worker1 in workers1:
                for worker2 in workers2:
                    param=AlgParametrs(name,minSup,minConf,worker1,worker2)
		    paramsList.append(param)
    return paramsList

class AlgParametrs(object):
    def __init__(self,name,minSup,minConf,datasetWorkers,itemsWorkers,time=0.0):
	self.name=name
        self.minSup=minSup
	self.minConf=minConf
	self.datasetWorkers=int(datasetWorkers)
	self.itemsWorkers=int(itemsWorkers)
	self.time=time

    def __str__(self):
        s=str(self.minSup)+","
        s+=str(self.minConf)+","
        s+=str(self.datasetWorkers)+","
        s+=str(self.itemsWorkers)+","
        s+=str(self.time)+"\n"
        return s

def getTime():
    return int(round(time.time() * 1000))

def execute(params,Dataset,path="/home/user/Desktop/ML2/"): 
    cmd ="erl -pa " + path +" -run apriori_cyclic extCall " 
    MinSup=str(params.minSup)
    MinConf=str(params.minConf)
    WorkersDataset=str(params.datasetWorkers) 
    WorkersItems=str(params.itemsWorkers) 
    #Dataset=data 
    postfix=" -run init stop -noshell"
    cmd+=MinSup + " " + MinConf +" " +WorkersDataset+" " +WorkersItems +" "
    cmd+=Dataset + postfix
    time1=getTime()
    os.system(cmd)
    time2=getTime()
    time =time2-time1
    return time

if __name__ == '__main__':
    createDataset(sys.argv[1],sys.argv[2])

