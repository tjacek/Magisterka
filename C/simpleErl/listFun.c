#include <stdlib.h>

typedef struct seq{
   double * array;
   int n;
} seq;

seq * makeSeq(int n){
    seq * x=(seq *) malloc(sizeof(seq));
    x->n=n;
    x->array=(double*) malloc(n*sizeof(double));
    int i;
    for(i=0;i<x->n;i++){
        x->array[i]=0.0;
    }
    return x;
}

double sum(seq * x){
    double s=0.0;
    int i;
    for(i=0;i<x->n;i++){
        s+=x->array[i];
    }
    return s;
}
