int ind(int i,int j,int nrow){
return i+j*nrow;
}

void matrix_product(AUTO1 *A, AUTO2 *B, AUTO3 *C, AUTO4 *dimA, AUTO5 *dimB, AUTO6 *dimC) {
  //Obtain the worker id
  int id = get_global_id(0);
  
  //Get the index for C
  int j=id/dimC[0];
  int i=id-j*dimC[0];
  
  //Compute the (i,j)th entry in C
  for(int k=0;k<dimA[1];k++){
    C[ind(i,j,dimC[0])]=C[ind(i,j,dimC[0])]+A[ind(i,k,dimA[0])]*B[ind(k,j,dimB[0])];
  }
}



