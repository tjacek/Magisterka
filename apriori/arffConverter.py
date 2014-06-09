import representation,callApriori,re,bow

def saveArffFile(expResult,pca,filename="apriori.arff"):
    arff=getArffFile(expResult,pca,False)
    arrfFile = open(filename,'w')
    arrfFile.write(arff)
    arrfFile.close()

def getArffFile(expResult,pca,discrete=False):
    arffFile="@RELATION aprioriDataset\n"
    arffFile=addAttributes(expResult.keys(),arffFile,pca,discrete)
    if(discrete):
	discretize(expResult)
    arffFile=addSamples(expResult,pca,arffFile)
    return arffFile

def addAttributes(datasets,arffFile,pca,discrete=False):
    stats=representation.getAttributes() 
    for attrName in stats:
	arffFile=addAttribute(attrName,arffFile)
    if pca!=None:
	for attrName in bow.pcaStats:
	    arffFile=addAttribute(attrName,arffFile)
    arffFile=addAttribute("minSup",arffFile)
    arffFile=addAttribute("minConf",arffFile)
    arffFile=addAttribute("workers",arffFile)
    if(discrete):
	arffFile+="@attribute class {negative,positive}\n"
    else:
    	arffFile=addAttribute("time",arffFile)
    return arffFile

def addAttribute(attrName,arffFile):
    attrName="@attribute "+ attrName + " numeric\n"
    arffFile+=attrName
    return arffFile

def addSamples(expResult,pca,arffFile):
    arffFile+="@data\n"    
    for dataset in expResult.keys():
        for instance in expResult[dataset]:
            sample=getStats(dataset)
            if pca!=None:
                stats=pca[dataset]
		sample+=bow.toStr(stats)
            sample+=instance.__str__()
            arffFile+=sample
    return arffFile 
        
def getStats(dataset):
    sample=""
    stats=representation.getStats(dataset)
    for key in stats.keys():
        sample+=str(stats[key])+","
    return sample

def parseArff(filename):
    arff=readArff(filename)
    attr,data=arff.split("@data\n")
    dim,attrNames=parseAttr(attr)
    points,labels=parsePoints(data)
    dataset=gen.createNewDataset(points,labels,dim)
    return dataset,attrNames

def readArff(filename):
    file=open(filename,'r')
    arff=file.read()
    file.close()
    return arff

def parseAttr(attr):
    lines=attr.split("\n")
    attrNames=[]
    reg = re.compile('@attribute(.)+numeric(.)*')
    dim=0
    for line in lines:
	if(reg.match(line)):
	    dim+=1
            name=extractAttrName(line)
            attrNames.append(name) 
    return dim,attrNames

def extractAttrName(line):
    line=line.replace("numeric","")
    return line.replace("@attribute","")

def parsePoints(data):
    lines=data.split("\n")
    lines=filter(lambda l: len(l)>0.0,lines)
    lines=map(lambda x:x.split(","),lines)
    labels=map(lambda x:x.pop(-1),lines)
    points=map(toFloat,lines)
    labels=map(toCat,labels)
    return points,labels

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

def toFloat(rawPoint):
    return map(float,rawPoint)

def toCat(raw):
    p = re.compile(r"true|false")
    if(p.match(raw)):
       if(raw=="true"):
  	   return 1.0
       return -1.0     
    return 0.0

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
