#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include "lsm.c"

using namespace std;


Samples * readFile(char * filename){
  ifstream infile(filename); // for example
  string line = "";
  int n=0,k=0; 
  while (getline(infile, line)){
    k=0;
    stringstream strstr(line);
    string word = "";
    while (getline(strstr,word, ',')){ 
         //cout << word << '\n';
         k++;
    }
    n++;
  }
  infile.close(); 
  Samples * samples=makeSamples(n,k);
  ifstream infile2(filename);
  int i=0,j=0;
  while (getline(infile2, line)){
    j=0;
    stringstream strstr(line);
    string word = "";
    while (getline(strstr,word, ',')){ 
      double temp = (double)atof(word.c_str());
      if(j==k-1){
	 samples->s[i]->y=temp;
      }else{
         samples->s[i]->x->data[j]=temp;
      }
      j++;   
    }
    i++;
  }
  infile2.close();
  return samples;
} 

int main(int argc,char * argv[]){
    if(argc<3){
        printf("Too few arguments \n");
        return 1;
    }
    //printf(" %s \n",argv[1]);
    Samples *train=readFile(argv[1]);
    //Samples * test=readFile(argv[2]);
    Vector  * theta=learn(train,0.00001,100.0);
    printVector(theta);
   // printSamples(samples);
    //printf(" %s \n",argv[2]);
    //printf(" %s \n",argv[3]);
    return 0;
}
