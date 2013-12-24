package pl.edu.agh.weka.benchmark;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import weka.core.Instances;

public class Dataparser {

	public static Instances parse(String filename){
		
         try {
        	BufferedReader reader = new BufferedReader(
                     new FileReader(filename)); 
            Instances data = new Instances(reader); 
            if (data.classIndex() == -1)
            	   data.setClassIndex(data.numAttributes() - 1);
			reader.close();
			return data;
		} catch (IOException e) {
			e.printStackTrace();
		}
         return null;
	}
}
