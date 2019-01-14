#include "C_interface_gcc.h"


void fun_finalizer(SEXP ext) {
	Rprintf("finalize\n");
	/* FIXME: check 'ext' is an external poointer */
	double *value = (double*)R_ExternalPtrAddr(ext);
	/* FIXME: check that value != NULL */
	free(value);
	/* FIXME: set R_ExternalPtrAddr(NULL) */
}



SEXP test() {
	double *x = (double*)malloc(10 * sizeof(double));

	SEXP answer = R_MakeExternalPtr((void *)x, NULL, NULL);
  printf("This is the end");
  //SEXP out = allocVector(REALSXP, 10);
	return(answer);
}