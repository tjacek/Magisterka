import representation,callApriori

def saveArffFile(expResult):
    arff=getArffFile(expResult,True)
    arrfFile = open('apriori.arff','w')
    arrfFile.write(arff)
    arrfFile.close()

def getArffFile(expResult,discrete=False):
    arffFile="@RELATION aprioriDataset\n"
    arffFile=addAttributes(expResult.keys(),arffFile,discrete)
    if(discrete):
	discretize(expResult)
    arffFile=addSamples(expResult,arffFile)
    return arffFile

def addAttributes(datasets,arffFile,discrete=False):
    stats=representation.getAttributes()
    for attrName in stats:
	arffFile=addAttribute(attrName,arffFile)
    arffFile=addAttribute("minSup",arffFile)
    arffFile=addAttribute("minConf",arffFile)
    arffFile=addAttribute("workers",arffFile)
    if(discrete):
	arffFile+="@ATTRIBUTE class {negative,positive}\n"
    else:
    	arffFile=addAttribute("time",arffFile)
    return arffFile

def addAttribute(attrName,arffFile):
    attrName="@attribute "+ attrName + " numeric\n"
    arffFile+=attrName
    return arffFile

def addSamples(expResult,arffFile):
    arffFile+="@DATA\n"    
    for dataset in expResult.keys():
        for instance in expResult[dataset]:
            sample=getStats(dataset)
            sample+=instance.__str__()
            arffFile+=sample
    return arffFile 
        
def getStats(dataset):
    sample=""
    stats=representation.getStats(dataset)
    for key in stats.keys():
        sample+=str(stats[key])+","
    return sample

def discretize(expResult):
    for dataset in expResult.values():
        average=0.0
        for result in dataset:
	    average+=result.time
        average/=len(dataset)
        threshold=1.1*average
	for result in dataset:
	    if(threshold<result.time):
		result.time="negative"
            else:
                result.time="positive"

example={'/home/user/Desktop/magisterka/apriori/datasets/mine.data': 1269L, '/home/user/Desktop/magisterka/apriori/datasets/gen.data': 1112L}
example2={
  '/home/user/Desktop/magisterka/apriori/datasets/mine.data': 
    callApriori.AprioriParametrs(0.5,0.5,1,1269L),
  '/home/user/Desktop/magisterka/apriori/datasets/gen.data' : 
    callApriori.AprioriParametrs(0.5,0.5,1,1112L)
}

if __name__ == '__main__':
    saveArffFile(example2)
#print(getArffFile(example))
