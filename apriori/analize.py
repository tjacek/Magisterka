import sys,math,os
sys.path.append("/home/user/Desktop/magisterka/data")
import attributesStats as stats,arff

filename="apriori.arff"
dirPath="/home/user/Desktop/magisterka/apriori/"

def analizeDataset(filename,path="stats/"):
    dirName=filename.replace(".arff","")
    fullPath=dirPath+path+dirName+"/"
    createDir(filename,path)
    fullText=matrixs(filename,fullPath)
    train,test=splitData(filename,fullPath)
    train=fullPath + train
    test=fullPath + test
    print(train+"\n")
    print(test+"\n")
    regression(train,test)
    #discretize("stats/",filename,interval)
    #classification(train,test)

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

def regression(trainFile,testFile):
    npRegression(trainFile,testFile)
    #callC(train,test) 
    #return train,test

def npRegression(trainFile,testFile,erlPath="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + erlPath +" -run regression run_exp "
    cmd+=trainFile + " " + testFile
    cmd+=" -run init stop -noshell "
    print(cmd)   
    os.system(cmd)

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

analizeDataset(filename)
