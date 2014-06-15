import arff,sys,math,os

def discretize(path,filename,category):
    dataset,attr=arff.parseArff(filename,True)
    for instance in dataset.instances:
        y=getPredVar(instance)
        cat=category(y)
        instance.removeLast()
        instance.setCategory(cat)
    dataset.dim-=1
    disc_filename=filename.replace(".arff","_disc.arff")
    arff.saveArff(dataset,path,disc_filename,disc_filename,getIntervalCategories())
    return disc_filename

def getPredVar(instance):
    return instance.point[instance.size-1]

def orderOfMagnidude(t,maxOrder=4):
    order=int(math.log(t,10))
    if(order<1):
        return 0
    if(t>maxOrder):
	return maxOrder
    return "order"+str(order)

def getMagnidudeCategories(n=4):
    cats=[]
    for i in range(0,n+1):
        cats.append("order"+str(i))
    return cats

def interval(t,order=4,maxIterval=4):
    order=math.pow(10.0,order)
    inter=int((maxIterval* t) / order)
    if(inter>maxIterval):
        inter=maxIterval
    return "interval"+str(inter)

def getIntervalCategories(n=4):
    cats=[]
    for i in range(0,n+1):
        cats.append("interval"+str(i))
    return cats
