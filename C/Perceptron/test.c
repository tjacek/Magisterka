#include <time.h>
#include "trainPerceptron.c"

double randomDouble(){
   double d=(double) (rand() % 1000);
   return d/100;
}

double linearPred(double * x){
   if(x[1]+x[2]>10.0){
       return 1.0;
   }else{
       return -1.0;
   }
}

Dataset * generateDataset(int n,int k,double (*pred)(double*)){
    srand(time(NULL));
    Dataset * d=makeDataset(n,k);
    int i,j;
    for(i=0;i<d->n;i++){
        for(j=1;j<d->k;j++){
            d->samples[i][j]=randomDouble();
        }
        d->labels[i]=pred(d->samples[i]);
    }
    return d;    
}

Dataset * separableDataset(int n){
    return generateDataset(n,2,linearPred);
}

int main(){
   Dataset * d=separableDataset(100);
   printDataset(d);
   Perceptron * p=train(d,0.01,0.1,1000);
   printPercept(p);
}
