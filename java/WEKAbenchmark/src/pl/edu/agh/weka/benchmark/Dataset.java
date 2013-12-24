package pl.edu.agh.weka.benchmark;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.trees.J48;
import weka.core.Instances;

public class Dataset {
	Instances train;
	Instances test;
    Classifier cModel = (Classifier)new NaiveBayes();
    
	public Dataset(Instances train,Instances test, Classifier cModel){
	    this.train=train;
	    this.test=test;
	    this.cModel=cModel;
	}
	
	public void experiment(){
		  try {
			  evaluate();
			} catch (Exception e) {
				e.printStackTrace();
			}
	}

	private void evaluate() throws Exception {
		cModel.buildClassifier(train);
		Evaluation eTest = new Evaluation(train);
		eTest.evaluateModel(cModel, test);
		String strSummary = eTest.toSummaryString();
		System.out.println(strSummary);
	}
	
	public static Dataset getDataset(String train,String test,boolean alg){
		Instances trainInst=Dataparser.parse(train);
		Instances testInst=Dataparser.parse(test);
		Classifier cModel;
        if(alg){
        	cModel = (Classifier)new NaiveBayes();
        }else{
        	cModel = (Classifier)new J48();

        }
        return new Dataset(trainInst,testInst,cModel);
	}

	public static void main(String [] args){
		Dataset data=getDataset("data/nonlinearTrain.arff","data/nonlinear.arff",true);
		data.experiment();
	}
}
