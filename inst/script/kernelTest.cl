AUTO3 add_element(AUTO1 *A, AUTO2 *B){
return *A+*B;
}

void vector_add(AUTO1 *A,  AUTO2 *B,  AUTO3 *C) {
// Get the index of the current element to be processed
int i = get_global_id(0);

// Do the operation
C[i] =add_element(A+i,B+i);
}


