__kernel void vector_add(__global AUTO1 *A, __global AUTO2 *B, __global AUTO3 *C) {

// Get the index of the current element to be processed
int i = get_global_id(0);

// Do the operation
C[i] =add(A[i],B[i]);
}

AUTO3 add(AUTO1 A, AUTO2 B){
return(A+B);
}

