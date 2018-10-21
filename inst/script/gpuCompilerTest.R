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


n=100
A=runif(n)
B=runif(n)
parms=list(ind=1:n,A=A,B=B)
test3<-function(ind,A,B){
  e=A[ind]*B[ind]
  return(e)
}


debugCode=.gpuSapply(1:n,test3,A,B)
cat(debugCode$gpu_code)
res=gpuSapply(1:n,test3,A,B)

res-A*B

library("tictoc")


n=5000
m=10000
k=1000
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)

test3<-function(ind,A,B){
  ind=ind-1
  j=floor(ind/nrow(A))
  i=ind-j*nrow(A)
  j=j+1
  i=i+1
  C=0
  for(k in 1:ncol(A)){
    C=C+A[i,k]*B[k,j]
  }
  return(C)
}

  


.gpuResourcesManager$setMaxMemLimit(4*10^9)
.gpuResourcesManager$getGPUusage()

tic()
res=gpuSapply(1:(n*k),test3,A,B)
toc()
tic()
res2=A%*%B
toc()
max(abs(res-res2))
#This will take your lifetime to finish.
getCurDevice()
getDeviceList()
setDevice(2)

n=1024
m=10000
k=1024
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)

test4<-function(ind,A,B){
  j=ind
  C=matrix(0,nrow(A),1)
  for(i in 1:nrow(A)){
    for(k in 1:ncol(A)){
      C[i]=C[i]+A[i,k]*B[k,j]
    }
  }
  return(C)
}

tic()
res=gpuSapply(1:k,test4,A,B)
toc()

tic()
res2=A%*%B
toc()
tic()
res1=sapply(1:k,test4,A,B)
toc()
max(abs(res-res2))




.gpuResourcesManager$getGPUusage()
