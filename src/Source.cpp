
#include "unitTestHeader.h"
void test1();
int main(void) {
	



	char* code = "double add_element(__global double *A,__global double *B){   return *A+*B; } __kernel void vector_add(__global  double *A,  __global  double *B,  __global  double *C) { int i = get_global_id(0); C[i]=A[i]+B[i];printf(\"%d:%f+%f=%d\"i,A[i],B[i],C[i]);  }";
	char* sig = "";
	char* kernel = "vector_add";
	char* flag = "";
	createKernel(&sig, &kernel, &code,&flag);
}


void test1() {

	
}
