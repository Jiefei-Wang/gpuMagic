#include "C_interface_gcc.h"



#include <stdlib.h>
#include <Rinternals.h>
SEXP test() {
  printf("start of test\n");
	double *x = (double*)malloc(10 * sizeof(double));
	printf("memory allocation finished\n");
	SEXP res = R_MakeExternalPtr((void *)x, NULL, NULL);
	printf("R object has been created\n");
	return(res);
}