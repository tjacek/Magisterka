import sys,math,os
sys.path.append("/home/user/Desktop/magisterka/data")
import attributesStats as stats,arff

filename="apriori.arff"

def analizeDataset(filename):
    #m1=stats.corlMatrix(filename)
    #save(m1,filename,prefix="corelation")
    #m2=stats.entropyMatrix(filename)
    #save(m2,filename,prefix="entropy")
    train,test=dataForRegression(filename)
    #discretize("stats/",filename,interval)
    classification(train,test)

def regression(filename):
    train,test=dataForRegression(filename)
    #callC(train,test) 
    return train,test

def dataForRegression(filename,path="/home/user/Desktop/magisterka/apriori/stats/"):
    dataset,attr=arff.parseArff(filename,True)
    train,test=arff.saveSplitedArff(dataset,"stats/",filename,False)
    dataset,attr=arff.parseArff(filename,True)
    train=saveRaw(dataset,path,train)
    test=saveRaw(dataset,path,test)
    return path+train,path+test

def callC(train,test,output="output.txt",path="/home/user/Desktop/ML/C/LSM/"):
    cmd=path +"lsm " + train +" " + test +" " + output
    os.system(cmd)

def saveRaw(dataset,path,filename):
    filename=filename.replace(".arff",".csv")
    arff.saveCsv(dataset,path,filename)
    return filename

def save(text,filename,prefix,path="stats/"):
    filename=filename.replace(".arff","_"+prefix+".txt")
    myFile = open(path+filename, 'w')
    myFile.write(text)
    myFile.close()

def classification(train,test):
    print(train)
    print(test)
    trainD=discretize("",train.replace(".csv",".arff"),interval)
    testD=discretize("",test.replace(".csv",".arff"),interval)
    callClass("naive_bayes",trainD,testD,output="stats/output.arff",stats="stats/stats.txt") 

def callClass(alg,train,test,output,stats,path="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + path +" -run test_classifer run_exp "
    cmd+=alg + " " + train
    cmd+=" " + test+" " + output +" " + stats
    cmd+=" -run init stop -noshell "   
    os.system(cmd)

def discretize(path,filename,category):
    dataset,attr=arff.parseArff(filename,True)
    for instance in dataset.instances:
        y=getPredVar(instance)
        cat=category(y)
        instance.removeLast()
        instance.setCategory(cat)
    disc_filename=filename.replace(".arff","_dics.arff")
    arff.saveArff(dataset,path,disc_filename,disc_filename,getIntervalCategories())
    return disc_filename

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
