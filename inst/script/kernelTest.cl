__kernel void vector_add(__global AUTO1 *A, __global AUTO2 *B, __global AUTO3 *C) {

// Get the index of the current element to be processed
int i = get_global_id(0);

// Do the operation
C[i] = A[i] + B[i];
}