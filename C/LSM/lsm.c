#include <stdio.h>
#include <stdlib.h>
#include "lsm_details.c"
#include <math.h> 

Vector * learn(Samples * s,double alpha,double maxErr);
double mse(Vector * theta,Samples * samples);
double applyRegression(Vector * theta,Vector * x);
void train(Vector * theta,Samples * samples,double alpha);
double gradient(Vector * theta,Samples * samples,int j);

Vector * learn(Samples * s,double alpha,double maxErr){
  Vector * theta=makeVector(s->n);
  int iter=0;
  while(mse(theta,s)>maxErr){
    if(iter>1000000){
      break;
    }
    //printf(" %f",mse(theta,s));
    //printVector(theta);
    train(theta,s,alpha);
    iter++;
  }
  return theta;
}

double mse(Vector * theta,Samples * samples){
  double serror=0.0;
  for(int i=0;i<samples->k;i++){
    Sample * s=samples->s[i];
    double predY=applyRegression(theta,s->x);
    double diff = predY - s->y;
    serror+= diff*diff;
  }
  double k=(double)samples->k;
  serror=serror/k;
  return sqrt(serror);
}

double applyRegression(Vector * theta,Vector * x){
  double value=0.0;
  for(int i=0;i<x->n;i++){
     value+=x->data[i]* theta->data[i];
  }
  return value;
}

void train(Vector * theta,Samples * samples,double alpha){
  for(int j=0;j<theta->n;j++){
    theta->data[j]-=  alpha * gradient(theta,samples,j);
  }
}

double gradient(Vector * theta,Samples * samples,int j){
  double grad=0.0;
  for(int i=0;i<samples->k;i++){
      Sample * s=samples->s[i];
      double trueY=s->y;
      double predY=applyRegression(theta,s->x);
      grad+= (predY-trueY) * s->x->data[j];
  }
  double k= (double) samples->k;
  grad= (2.0/k) * grad;
  return grad;
}
