# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 19:42:24 2013

@author: tjacek
"""
def execute(filename,path="/home/user/Desktop/ML/src"):
    cmd ="erl -pa " + path +" -run apriori extCall "
    print(cmd) 
