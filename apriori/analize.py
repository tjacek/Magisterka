import sys,math
sys.path.append("/home/user/Desktop/magisterka/data")
import attributesStats as stats,arff

filename="apriori.arff"

def analizeDataset(filename):
    #m1=stats.corlMatrix(filename)
    #save(m1,filename,prefix="corelation")
    #m2=stats.entropyMatrix(filename)
    #save(m2,filename,prefix="entropy")
    #regression(filename)
    discretize("stats/",filename,interval)

def discretize(path,filename,category):
    dataset,attr=arff.parseArff(filename,True)
    for instance in dataset.instances:
        y=getPredVar(instance)
        cat=category(y)
        instance.removeLast()
        instance.setCategory(cat)
    disc_filename=filename.replace(".arff","_dics.arff")
    arff.saveArff(dataset,path,disc_filename,disc_filename,getIntervalCategories())

def regression(filename):
    dataset,attr=arff.parseArff(filename,True)
    train,test=arff.saveSplitedArff(dataset,"stats/",filename,False)

def save(text,filename,prefix,path="stats/"):
    filename=filename.replace(".arff","_"+prefix+".txt")
    myFile = open(path+filename, 'w')
    myFile.write(text)
    myFile.close()

def getPredVar(instance):
    return instance.point[instance.size-1]

def orderOfMagnidude(t,maxOrder=4):
    order=int(math.log(t,10))
    if(order<1):
        return 0
    if(t>maxOrder):
	return maxOrder + 1
    return "order"+str(order)

def getMagnidudeCategories(n=4):
    cats=[]
    for i in range(0,n+1):
        cats.append("order"+str(i))
    return cats

def interval(t,order=4,maxIterval=4):
    order=math.pow(10.0,order)
    inter=int((maxIterval* t) / order)
    if(inter>order):
        inter=order+1
    return "interval"+str(inter)

def getIntervalCategories(n=4):
    cats=[]
    for i in range(0,n+1):
        cats.append("interval"+str(i))
    return cats

analizeDataset(filename)
