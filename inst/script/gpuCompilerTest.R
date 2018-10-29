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

code="__kernel void gpu_kernel0(__global double* gpu_worker_data,__global double* A,__global double* B,__global float* gpuMagic_tmp,__global long* gpu_tmp_length_arg,__global long* gpu_matrix_offSize,__global long* gpu_matrix_size1,__global long* gpu_matrix_size2,__global double* gpu_return_variable,__global long* gpu_return_size){\nunsigned long gpu_global_id=get_global_id(0);\nunsigned long gpu_tmp_length=*gpu_tmp_length_arg;\nunsigned long gpu_worker_offset=gpu_global_id*gpu_tmp_length;\ndouble opencl_tmp_1;\ndouble ind;\ndouble j;\nint opencl_tmp_2;\n__global double* C=(__global double*)(gpuMagic_tmp+gpu_worker_offset+gpu_matrix_offSize[0]);\nint opencl_tmp_3;\nint opencl_tmp_4;\ndouble opencl_tmp_5;\ndouble opencl_tmp_7;\ndouble opencl_tmp_8;\ndouble opencl_tmp_6;\ndouble opencl_tmp_9;\nopencl_tmp_1=gpu_global_id+1;\nind=gpu_worker_data[(unsigned int)opencl_tmp_1-1];\nj=ind;\nopencl_tmp_2=gpu_matrix_size1[1];\n\nfor(unsigned int gpu_loop_ind_0=1;gpu_loop_ind_0<=opencl_tmp_2;gpu_loop_ind_0++){\nfor(unsigned int gpu_loop_ind_1=1gpu_loop_ind_1<=1;gpu_loop_ind_1++){\nC[(unsigned int)gpu_loop_ind_0-1 +((unsigned int)gpu_loop_ind_1-1)*gpu_matrix_size1[12]]=0;\n}\n}\nopencl_tmp_3=gpu_matrix_size1[1];\nfor(unsigned int gpu_loop_ind_2=1;gpu_loop_ind_2<=opencl_tmp_3;gpu_loop_ind_2++){\nopencl_tmp_4=gpu_matrix_size2[1];\nfor(unsigned int gpu_loop_ind_3=1;gpu_loop_ind_3<=opencl_tmp_4;gpu_loop_ind_3++){\nopencl_tmp_5=C[(unsigned int)gpu_loop_ind_2-1];\nopencl_tmp_7=A[(unsigned int)gpu_loop_ind_2-1 +((unsigned int)gpu_loop_ind_3-1)*gpu_matrix_size1[1]];\nopencl_tmp_8=B[(unsigned int)gpu_loop_ind_3-1 +((unsigned int)j-1)*gpu_matrix_size1[2]];\nopencl_tmp_6=opencl_tmp_7*opencl_tmp_8;\nopencl_tmp_9=opencl_tmp_5+opencl_tmp_6;\nC[(unsigned int)gpu_loop_ind_2-1]=opencl_tmp_9;\n}\n}\nfor(unsigned long gpu_return_i=0;gpu_return_i<*gpu_return_size;gpu_return_i++){\n\ngpu_return_variable[gpu_return_i+gpu_global_id*(*gpu_return_size)]=C[gpu_return_i];\n}\n}"
gpuSapply(1:(n*k),matMul,A,B,debugCode = code)


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




findMaxInd<-function(colInd,A,k1){
  k=k1[1]
  maxInd=gMatrix(nrow=k,ncol=1,location="private")
  maxNum=gMatrix(nrow=k,ncol=1,location="private")
  for(i in 1:k){
    maxNum[i]=0
  }
  for(i in 1:nrow(A)){
    
    a=A[i,colInd]
    for(j in 1:k){
      if(a>maxNum[j]){
        if(j!=k){
          j1=j+1
          for(t in j1:k){
            ind=k+j-t
            maxNum[ind+1]=maxNum[ind]
            maxInd[ind+1]=maxInd[ind]
          }
        }
        maxNum[j]=a
        maxInd[j]=i
        a=0
      }
    }
  }
  return(maxInd)
}


n=10
m=10
topNum=2
A=matrix(runif(n*m),n,m)


microbenchmark(
  GPU=gpuSapply(1:ncol(A),findMaxInd,A,topNum),
  #CPU1=sapply(1:ncol(A),findMaxInd,A,topNum),
  CPU2=apply(A,2,function(x){
    which(rank(x)==length(x)-1)
  }),
  times=1
)

a=(GPU-CPU2)

A[9,1]
A[10,1]

res=gpuSapply(1:ncol(A),findMaxInd,A,topNum)
res1=sapply(1:ncol(A),findMaxInd,A,topNum)
which(abs(res-res1)!=0,arr.ind=T)
res2=apply(A,2,function(x){
  which(rank(x)==length(x)-1)
})


getCurDevice()
getDeviceList()
setDevice(2)


