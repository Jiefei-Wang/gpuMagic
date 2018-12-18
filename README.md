# gpuMagic
An R interface to write OpenCL functions

This package provide an easy way to access the power of the GPU using the familar R language. Since it is still under development, tons of bugs is hidden somewhere and waiting to surprise someone. Please use it with your best judgement.

Before you start to use the package, please make sure you have the CUDA toolkit installed. Even though the package uses openCL as the background language, the current makevars only support CUDA path. If you have another openCL platform, you need to customize the makevars to compile the package.

After installed the package, here is an example about how to use gpu to compute the matrix multiplication:

```
#C=A%*%B
MatMul<-function(ind,A,B){
  tmp=B[,ind]
  C=A%*%tmp
  return(C)
}

#Initialize the data
n=1024
m=10000
k=1024
A=matrix(runif(n*m),n,m)
B=matrix(runif(m*k),m,k)

#The traditional way to compute the matrix multiplication
res_cpu_stupid=sapply(1:k,MatMul,A,B)
res_cpu_clever=A%*%B

#Use the GPU to run the code
res_gpu_genius=gpuSapply(1:n,MatMul,A,B)

#compute the error
range(res_gpu_genius-res_cpu_stupid)
range(res_gpu_genius-res_cpu_clever)
```

Using the GPU compute, you can expect around 3-6X speedup compared with the optimal CPU counterpart function(e.g %*%). However, since it is not easy to find the optimal CPU code when you are developing your own algorithm, it is more common that you have to rely on the sapply function to loop your code, therefore you will get much more benefit by using this package.


