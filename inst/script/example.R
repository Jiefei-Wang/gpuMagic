getDeviceList()
getCurDevice()
setDevice(0)

library("tictoc")
fileName <- 'inst/script/kernelMatrix.cpp'

#C=A%*%B
n=200
A=matrix(runif(n*n),n,n)
B=matrix(runif(n*n),n,n)
C=matrix(0,n,n)


##GPU computing(GeForce 940M)
tic()
#Allocate GPU memory
dev_C=gpuMatrix(C,T_F64)
.kernel(file=fileName,kernel="matrix_product",parms=list(A,B,dev_C,dim(A),dim(B),dim(C)),globalThreadNum = length(C))
dev_C=sync(dev_C)
toc()

##R internel function
tic()
C=A%*%B
toc()

#The differences between GPU and R
max(abs(as.matrix(dev_C)-C))



##GeForce 940M
#2.7 sec
##Intel(R) HD Graphics 520
#3.47 sec
##Intel(R) Core(TM) i5-6300U CPU
#16 sec
##Rcpp
#17 sec(Without compilation time)
##R matrix function
#5 sec



n=23863
m=183
rank=50
test_W=matrix(runif(n*rank,0,1),n,rank)
test_H=matrix(runif(rank*m,0,1),rank,m)
gpu_W=gpuMatrix(test_W)
gpu_H=gpuMatrix(test_H)
gpu_C=gpuMatrix(matrix(0,n,m))

.kernel(file=fileName,kernel="matrix_product",parms=list(gpu_W,gpu_H,gpu_C,dim(gpu_W),dim(gpu_H),dim(gpu_W)),globalThreadNum = length(gpu_C))

gpu_C=download(gpu_C)
gpu=as.matrix(gpu_C)

cpu=test_W%*%test_H
max(abs(gpu-cpu))







#Force releasing the GPU memory
.gpuResourcesManager$getGPUusage()
.gpuResourcesManager$releaseAll()
.gpuResourcesManager$getGPUusage()

#Auto garbage collection
.gpuResourcesManager$setMaxMemLimit(20)
.gpuResourcesManager$getGPUusage()
dev_matrix=gpuMatrix(c(1),T_F64)
.gpuResourcesManager$getGPUusage()
dev_matrix=gpuMatrix(c(1),T_F64)
.gpuResourcesManager$getGPUusage()
dev_matrix=gpuMatrix(c(1),T_F64)
.gpuResourcesManager$getGPUusage()
gc()
.gpuResourcesManager$getGPUusage()
rm(list=ls())
gc()
.gpuResourcesManager$getGPUusage()
