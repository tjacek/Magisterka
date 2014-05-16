# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 19:42:24 2013

@author: tjacek
"""
import os,arff

def execute(trainfile,path="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + path +" -run test_classifer run_exp "
    cmd+=" " + trainfile
    testfile=  trainfile.replace("Train.arff","Test.arff")
    cmd+=" " + testfile
    outputfile=  trainfile.replace("Train.arff","Output.arff")
    cmd+=" " + outputfile
    cmd+=" -run init stop -noshell "
    os.system(cmd)
    arff.prepareOutput(outputfile)
    #print(cmd)
 
