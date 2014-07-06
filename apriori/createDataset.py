import sys,os,bow
sys.path.append("/home/user/Desktop/magisterka/data")
import arffConverter as arff,callApriori as call

def createDataset(filename,dirName):
    items=getFilenames(dirName)
    results=experiment(items)     
    arff.saveArffFile(results,None,filename)

def createPcaDataset(filename,dirName):
    items=getFilenames(dirName)
    pca=bow.getStats(items)
    results=experiment(items)    
    arff.saveArffFile(results,pca,filename)

def getFilenames(dirName):
    items = [ f for f in os.listdir(dirName)]
    currDir=os.getcwd()
    return map(lambda x: currDir+"/"+dirName+"/"+x,items)

def experiment(datasets):
    results={}
    i=0
    for datasetName in datasets:
        print("Dataset " + str(i) +"\n")
        expsForDataset=[]
        minSup=[0.1,0.4,0.8]
        minConf=[0.2,0.4,0.7]
        workers=[1,2,4]
        args=getArguments(datasetName,minSup,minConf,workers)
        for params in args:
 	    time=execute(params,datasetName)
            params.time=time
            expsForDataset.append(params)
        results[datasetName]=expsForDataset
        i+=1
    return results

def getArguments(name,minSupValues,minConfValues,workersValues):
    paramsList=[]
    for minSup in minSupValues:
	for minConf in minConfValues:
            for workers in workersValues:
                param= call.AprioriParametrs(name,minSup,minConf,int(workers))
		paramsList.append(param)
    return paramsList

def execute(args,dataset):
    return call.execute(args.minSup,args.minConf,args.workers,dataset)

if __name__ == '__main__':
    createPcaDataset(sys.argv[1],sys.argv[2])


