GPUcode2$gpu_code="
  __kernel void gpu_kernel0(__global double* gpu_worker_data,__global double* A,__global double* B,__global float* gpuMagic_tmp,__global long* gpu_tmp_length_arg,__global long* gpu_matrix_offSize,__global long* gpu_matrix_size1,__global long* gpu_matrix_size2,__global double* gpu_return_variable,__global long* gpu_return_size){
unsigned long gpu_global_id=get_global_id(0);
  unsigned long gpu_tmp_length=*gpu_tmp_length_arg;
  unsigned long gpu_worker_offset=gpu_global_id*gpu_tmp_length;
  double ind ;
  double opencl_tmp_1 ;
  double opencl_tmp_2 ;
  double e ;
  ind=gpu_worker_data[gpu_global_id];

  opencl_tmp_1=A[(int)ind-1];
printf(\"%f,\",ind);
  //opencl_tmp_2=B[(unsigned int)ind-1];
  //e=opencl_tmp_1+opencl_tmp_2;
  //gpu_return_variable[gpu_global_id]=e;
}
  "


library("microbenchmark")








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

  
n=10000
m=5000
k=1000
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)


getDeviceList()
setDevice(2)

code=compileGPUCode(1:(n*k),matMul,A,B)
cat(code$gpu_code)




res=gpuSapply(1:(n*k),matMul,A,B)
res1=sapply(1:(n*k),matMul,A,B)

res2=A%*%B

max(abs(res-res2))


microbenchmark(
  res=gpuSapply(1:(n*k),matMul,A,B),
  res2=A%*%B,
  times = 10
)



getCurDevice()
getDeviceList()
setDevice(2)


n=1000
m=10000
k=1000
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)

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




res=gpuSapply(1:k,matMul2,A,B)
res2=A%*%B
max(abs(res-res2))

microbenchmark(
  res=gpuSapply(1:k,matMul2,A,B),
  res2=A%*%B,
  times = 2
)




n=1000
m=1000
k=10000
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)
matMul3<-function(ind,A,B){
  tmp=gMatrix(nrow=nrow(B),location="private")
  j=ind
  C=matrix(0,nrow(A),1)
  for(i in 1:nrow(B)){
    tmp[i]=B[i,j]
  }
  
  for(i in 1:nrow(A)){
    for(k in 1:ncol(A)){
      C[i]=C[i]+A[i,k]*B[k,j]
    }
  }
  return(C)
}

code=compileGPUCode(1:k,matMul3,A,B)
cat(code$gpu_code)
microbenchmark(
res=gpuSapply(1:k,matMul3,A,B),
res2=A%*%B,
times=2
)
max(abs(res-res2))



findMaxInd<-function(k,A){
  maxInd=gMatrix(nrow=2,ncol=1,location="private")
  maxNum=gMatrix(nrow=2,ncol=1,location="private")
  for(i in 1:2){
    maxNum[i]=0
  }
  for(i in 1:nrow(A)){
    a=A[i,k]
    if(a>maxNum[1]){
      maxNum[2]=maxNum[1]
      maxInd[2]=maxInd[1]
      maxNum[1]=a
      maxInd[1]=i
    }else{
      if(a>maxNum[2]){
        maxNum[2]=a
        maxInd[2]=i
      }
    }
  }
  return(maxInd)
}


n=1000
m=10000
A=matrix(runif(n*m),n,m)

code=compileGPUCode(1:ncol(A),findMaxInd,A)



