package pl.edu.agh.weka.benchmark;

import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instances;

public class DataCreator {
	static final String name="TestData";
	static final String cat="cat";
    static String[] keywords = { "x", "y","z"};
	static final int numberOfAttributes = 4;
	
	public static Instances getInstances(){
		return new Instances(name, getAttr(),numberOfAttributes);
	}
	
	private static FastVector getAttr() {
        FastVector attributes = new FastVector(keywords.length + 1);
        for (int i = 0 ; i < keywords.length; i++) {
            attributes.addElement(new Attribute(keywords[i]));
          }
        final FastVector classValues = new FastVector(2);
        classValues.addElement("true");
        classValues.addElement("false");
        attributes.addElement(new Attribute(cat, classValues));
		return attributes;
	}
}
