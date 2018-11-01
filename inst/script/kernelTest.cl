double add_element(global double *A,global double *B){
  return *A+*B;
}

kernel void vector_add(global  double *A,  global  double *B,  global  double *C) {
// Get the index of the current element to be processed
int i = get_global_id(0);


// Do the operation
//C[i] =add_element(A+i,B+i);
C[i]=A[i]+B[i];
}


