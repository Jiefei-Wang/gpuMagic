#C=A%*%B
matMul<-function(id,A,B){
  #find the index of the entry of C matrix
  id=id-1
  j=floor(id/nrow(A))
  i=id-j*nrow(A)
  j=j+1
  i=i+1
  #initialize C[i,j]
  C=0
  for(k in 1:ncol(A)){
    C=C+A[i,k]*B[k,j]
  }
  return(C)
}


getDeviceList()
setDevice(0)
getCurDevice()

n=1000
m=1000
k=10000
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)


microbenchmark(
  res=gpuSapply(1:(n*k),matMul,A,B),
  res2=A%*%B,
  times = 1
)


#==========================================
#C=A%*%B
matMul2<-function(ind,A,B){
  j=ind
  C=matrix(0,nrow(A),1)
  for(i in 1:nrow(A)){
    tmp=0
    for(k in 1:ncol(A)){
      tmp=tmp+A[i,k]*B[k,j]
    }
    C[i]=tmp
  }
  return(C)
}

getDeviceList()

microbenchmark(
  res=gpuSapply(1:k,matMul2,A,B),
  res2=A%*%B,
  times = 1
)
