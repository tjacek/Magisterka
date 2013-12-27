#include <stdio.h>
#include <stdlib.h>
#include "perceptron.c"

typedef struct Dataset{
   int n;
   int k;
   double ** samples;
   double * labels;
} Dataset;

double x(Dataset * d,int i,int j){
    return d->samples[i][j];
}

Dataset * makeDataset(int n,int k){
    Dataset *d=(Dataset*) malloc(sizeof(Dataset));
    d->n=n;
    d->k=k;
    int i,j;
    d->samples=(double **) malloc(n*sizeof(double*));
    d->labels=(double *) malloc(n*sizeof(double));
    for(i=0;i<n;i++){
        d->samples[i]=(double *) malloc(k*sizeof(double));
        for(j=0;j<k;j++){
            d->samples[i][j]=0.0; 
        }
        d->labels[i]=0.0;
    }
    return d;
}

void printDataset(Dataset * d){
   int i,j;
   for(i=0;i<d->n;i++){
       for(j=0;j<d->k;j++){
           printf(" %f",x(d,i,j));
       }
       printf(" %f \n",d->labels[i]);
   }
}

double * currentOutput(Dataset *d,Perceptron * p){
    double * y=(double*) malloc(d->n*sizeof(double));
    int i;
    for(i=0;i<d->n;i++){
        y[i]=activation(p,d->samples[i]);
    }
    return y;
}

void updateWeights(Dataset * d,Perceptron * p,double * y,double alpha){
    int i,j;
    for(i=0;i<p->n;i++){
        double update=0.0;
        for(j=0;j<d->k;j++){
            update+=(d->labels[j]-y[j]) *x(d,i,j);
        }
        update*=alpha;
        p->w[i]+=update;
    }
}

double * step(Dataset * d,Perceptron * p,double alpha){
    double * y=currentOutput(d,p);
    updateWeights(d,p,y,alpha);
    return y;
}

double error(Dataset * d,double * y){
    double err=0.0;
    double * l=d->labels;
    int i;
    for(i=0;i<d->n;i++){
        err=abs(y[i] - l[i]);
    }
    err/=d->n; 
    return err;
}

Perceptron * train(Dataset * d,double alpha,int maxIter){
    int iter=0;
    Perceptron * p=makePerceptron(d->k);
    while(iter<maxIter){
        double * y=step(d,p,alpha);
        printf("%d %f \n",iter,error(d,y));
        iter+=1;
    }
    return p;
}
