#include <stdio.h>
#include <stdlib.h>

typedef struct Perceptron{
   int n;
   double * w;
   double threshold;
} Perceptron;

void clean(Perceptron * p){
    int i;
    for(i=0;i<p->n;i++){
       p->w[i]=0.0;
    }
}

Perceptron * makePerceptron(int n){
    Perceptron * p=(Perceptron*) malloc(sizeof(Perceptron));
    p->n=n;
    p->w= malloc(n*sizeof(double));
    p->threshold=0.0;
    clean(p);
    return p;
}

double sum(Perceptron * p,double * x){
    double u=0.0;
    int i;
    for(i=0;i<p->n;i++){
        u+=p->w[i]*x[i];
    }
    return u;
}

int activation(Perceptron * p,double * x){
    double u=sum(p,x);
    if(u>p->threshold){
        return 1.0;
    }else{
        return -1.0;
    }
} 


void printPercept(Perceptron * p){
   int i;
   for(i=0;i<p->n;i++){
      printf(" %f",p->w[i]);
   }
   printf("\n");
}

/*int main(){
   Perceptron * p=makePerceptron(3);
   double x[3]={1.0,2.0,3.0};
   printPercept(p); 
   activation(p,x);
}*/
