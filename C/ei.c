#include "erl_interface.h"
#include "ei.h"
#include "io.c"

typedef unsigned char byte;
static  Perceptron * p;
static int i=0;

double crt(int n,int k){
    p = createPerceptron(n,k);
    return 1.0;
}

double per(){
    if(i<p->n){
       double wk=p->w[i];
       i++;
       return wk;
    }
    return (double)p->n;
}



double callFunction(ETERM *fnp,ETERM *argp1,ETERM *argp2){
   if(strncmp(ERL_ATOM_PTR(fnp), "crt", 3) == 0) {
      int n=ERL_INT_VALUE(argp1);
      int k=ERL_INT_VALUE(argp2);
      return crt(n,k);
   }
   if (strncmp(ERL_ATOM_PTR(fnp), "per", 17) == 0) {
      return per();
   }
   return 0.0;
}

int main() {
  ETERM *tuplep, *intp;
  ETERM *fnp, *argp1,*argp2;
  double res;
  byte buf[100];
  long allocated, freed;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp1 = erl_element(2, tuplep);
    argp2 = erl_element(3, tuplep);
    res=callFunction(fnp,argp1,argp2);

    intp = erl_mk_float(res);
    erl_encode(intp, buf);
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp1);
    erl_free_term(argp1);
    erl_free_term(intp);
  }
}
