import representation

def getArffFile(expResult):
    arffFile="@RELATION aprioriDataset\n"
    arffFile=addAttributes(expResult.keys(),arffFile)
    arffFile=addSamples(expResult,arffFile)
    return arffFile

def addAttributes(datasets,arffFile):
    stats=representation.getStats(datasets[0])
    for attrName in stats.keys():
	arffFile=addAttribute(attrName,arffFile)
    arffFile=addAttribute("time",arffFile)
    return arffFile

def addAttribute(attrName,arffFile):
    attrName="@attribute "+ attrName + " numeric\n"
    arffFile+=attrName
    return arffFile

def addSamples(expResult,arffFile):
    arffFile+="@DATA\n"
    for dataset in expResult.keys():
        sample=getStats(dataset)
        sample+=str(expResult[dataset]) + "\n"
        arffFile+=sample
    return arffFile 
        
def getStats(dataset):
    sample=""
    stats=representation.getStats(dataset)
    for key in stats.keys():
        sample+=str(stats[key])+","
    return sample

example={'/home/user/Desktop/magisterka/apriori/datasets/mine.data': 1269L, '/home/user/Desktop/magisterka/apriori/datasets/gen.data': 1112L}

print(getArffFile(example))
