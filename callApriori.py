import os

def execute(sup,conf,work,data,path): 
    cmd ="erl -pa " + path +" -run apriori extCall " 
    MinSup=str(sup)
    MinConf=str(conf)
    Workers=str(work) 
    Dataset=data 
    postfix=" -run init stop -noshell"
    cmd+=MinSup + " " + MinConf +" " +Workers+" "
    cmd+=Dataset + postfix
    print(cmd)
    os.system(cmd)

path="/home/user/Desktop/ML/src"
execute(0.5,0.5,1,"src/mine.data",path)
