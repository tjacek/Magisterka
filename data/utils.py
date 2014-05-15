# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 19:42:24 2013

@author: tjacek
"""
import os

def execute(filename,path="/home/user/Desktop/ML/src"):
    cmd="erl -pa " + path +" -run test_classifer run_exp "
    cmd+=" " + filename
    cmd+=" -run init stop -noshell "
    os.system(cmd)
    print(cmd) 
