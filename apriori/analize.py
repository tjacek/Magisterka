import sys
sys.path.append("/home/user/Desktop/magisterka/data")
import attributesStats as stats,arff

filename="apriori.arff"

def analizeDataset(filename):
    m1=stats.corlMatrix(filename)
    save(m1,filename,prefix="corelation")
    m2=stats.entropyMatrix(filename)
    save(m2,filename,prefix="entropy")
    regression(filename)

def regression(filename):
    dataset,attr=arff.parseArff(filename,True)
    #print(dataset)
    train,test=arff.saveSplitedArff(dataset,"stats/",filename)

def save(text,filename,prefix,path="stats/"):
    filename=filename.replace(".arff","_"+prefix+".txt")
    myFile = open(path+filename, 'w')
    myFile.write(text)
    myFile.close()

analizeDataset(filename)
