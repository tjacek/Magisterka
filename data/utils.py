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
    outputfile=  trainfile.replace("Train.arff","_"+algName+".arff")
    cmd+=" " + outputfile
    statsfile= path.replace("src","stats/output_"+algName+".txt")
    cmd+=" " + statsfile
    cmd+=" -run init stop -noshell "   
    os.system(cmd)
    arff.prepareOutput(testfile,outputfile)
 
