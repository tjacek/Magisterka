import sys,math,os
sys.path.append("/home/user/Desktop/magisterka/data")
import attributesStats as stats,arff,discretization as disc

filename="first.arff"
dirPath="/home/user/Desktop/magisterka/apriori/"

def analizeDataset(filename,path="stats/"):
    dirName=filename.replace(".arff","")
    fullPath=dirPath+path+dirName+"/"
    #createDir(filename,path)
    #fullText=matrixs(filename,fullPath)
    train,test=splitData(filename,fullPath)
    train=fullPath + train
    test=fullPath + test
    #regression(train,test,fullPath+dirName,fullPath)
    categories=disc.getMagnidudeCategories()
    trainD,testD=discretize (train,test,fullPath,disc.orderOfMagnidude,categories)
    classification(trainD,testD)

def createDir(filename,path):
    dirName=filename.replace(".arff","")
    fullPath=path+dirName
    os.system("mkdir "+fullPath)

def matrixs(filename,path):
    m1=stats.corlMatrix(filename)
    save(m1,filename,"corelation",path)
    m2=stats.entropyMatrix(filename)
    save(m2,filename,"entropy",path)
    return path+filename

def save(text,filename,prefix,path):
    fullName=filename.replace(".arff","_"+prefix+".txt")
    fullPath=path+fullName
    myFile = open(fullPath, 'w')
    myFile.write(text)
    myFile.close()

def splitData(filename,path="/stats/apriori/"):
    dataset,attr=arff.parseArff(filename,True)
    train,test=arff.saveSplitedArff(dataset,path,filename,False)
    return train,test

def regression(train,test,dirName,path):
    npRegression(train,test,dirName)
    regressionC(train,test,dirName,path)

def npRegression(trainFile,testFile,dirName,erlPath="/home/user/Desktop/ML/src"):
    output=dirName+"_npr.txt"
    cmd="erl -pa " + erlPath +" -run regression run_exp "
    cmd+=trainFile + " " + testFile +" "+ output
    cmd+=" -run init stop -noshell "
    print(cmd)   
    os.system(cmd)

def regressionC(train,test,dirName,path):
    trainCSV,testCSV=dataForC(train,test,path)
    output=dirName+"_lsm.txt"
    callC(trainCSV,testCSV,output)

def dataForC(train,test,path):
    trainCSV=toCSV(train,path)
    testCSV=toCSV(test,path)
    return trainCSV,testCSV

def toCSV(filename,path):
    dataset,attr=arff.parseArff(filename,True)
    return saveRaw(dataset,path,filename)

def callC(train,test,output="output.txt",cpath="/home/user/Desktop/ML/C/LSM/"):
    print(train)
    cmd=cpath +"lsm " + train +" " + test +" " + output
    os.system(cmd)

def saveRaw(dataset,path,filename):
    filename=filename.replace(".arff",".csv")
    arff.saveCsv(dataset,"",filename)
    return filename

def discretize(train,test,fullPath, category,categories):
     print(train)
     trainDisc=disc.discretize("",train,category,categories) 
     testDisc =disc.discretize("",test,category,categories) 
     return trainDisc,testDisc

def classification(train,test):
    algs=["c45","naive_bayes","nearest_neighbors"]  
    for alg in algs:
        output=test.replace("Test_disc.arff","_"+alg+".arff")
        stats=test.replace("Test_disc.arff","_"+alg+"_stats.txt")
        print(alg +" " + train +" " +test+" "+output+" "+stats)
        callClass(alg,train,test,output,stats) 

def callClass(alg,train,test,output,stats,path="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + path +" -run test_classifer run_exp "
    cmd+=alg + " " + train
    cmd+=" " + test+" " + output +" " + stats
    cmd+=" -run init stop -noshell "   
    os.system(cmd)

analizeDataset(filename)
