#include "erl_interface.h"
#include "ei.h"
#include "listFun.c"

typedef unsigned char byte;
static seq * x;
static int k;

int crt(int n){
    x=makeSeq(n);
    return 1;
}

int add(int value){
    if(k<x->n){
        k++;
        x->array[k]=(double) value;
    }
    return (int) sum(x);
}



int callFunction(ETERM *fnp,ETERM *argp){
   if(strncmp(ERL_ATOM_PTR(fnp), "crt", 3) == 0) {
      return crt(ERL_INT_VALUE(argp));
   }
   if (strncmp(ERL_ATOM_PTR(fnp), "add", 17) == 0) {
      return add(ERL_INT_VALUE(argp));
   }
   return 0;
}

int main() {
  ETERM *tuplep, *intp;
  ETERM *fnp, *argp;
  int res;
  byte buf[100];
  long allocated, freed;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    res=callFunction(fnp,argp);

    intp = erl_mk_int(res);
    erl_encode(intp, buf);
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
    erl_free_term(intp);
  }
}
