# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 19:42:24 2013

@author: tjacek
"""
import os,arff,copy

algorithms = ["c45","naive_bayes","nearest_neighbors"]

def execute(algName,trainfile,path="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + path +" -run test_classifer run_exp "
    cmd+=algName + " " + trainfile
    testfile= copy.copy(trainfile).replace("Train.arff","Test.arff")
    cmd+=" " + testfile
    outputfile=  trainfile.replace("Train.arff",algName+"_Output.arff")
    cmd+=" " + outputfile
    cmd+=" -run init stop -noshell "
    #print(cmd)
    os.system(cmd)
    arff.prepareOutput(testfile,outputfile)
 
