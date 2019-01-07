library("microbenchmark")
setDevice(c(1,2,3))
.gpuResourcesManager$getGPUusage()
n=4000
type="double"
A=runif(n)
A_dev=gpuMatrix(A,type=type)
fileName="inst/script/performance test code.c"
opt=kernel.getOption()
#opt$verbose=T
.kernel(src = fileName,kernel="test_vector",.globalThreadNum = n/4,
        parms=list(A_dev,1),.options = opt)
A_dev=download(A_dev)
A1=as.matrix(A_dev)
range(2*sqrt(A)-A1)





stride=4
n=stride*10240000
type="float"
A=runif(n)
A_dev=gpuMatrix(A,type=type)
B_dev=gpuEmptMatrix(n,1,type=type)
off=gpuMatrix(stride,type="int")
fileName="inst/script/performance test code.c"
opt=kernel.getOption()
#opt$verbose=T
microbenchmark({
  .kernel(file = fileName,kernel="strideCopy",.globalThreadNum = n/stride,
          parms=list(B_dev,A_dev,off),.options = opt)
  off=download(off)
},
times = 10
)
ind=seq(1,n,by=stride)
B_dev=download(B_dev)
B=as.matrix(B_dev)
range(A[ind]-B[ind])



n=10240000*5
type="float"
A=runif(n)
A_dev=gpuMatrix(A,type=type)
B_dev=gpuEmptMatrix(n,1,type=type)
pos=gpuMatrix(0,type="int")
fileName="inst/script/performance test code.c"
opt=kernel.getOption()
#opt$verbose=T
microbenchmark({
  .kernel(file = fileName,kernel="multiPosCopy",.globalThreadNum = n,
          parms=list(B_dev,A_dev,pos),.options = opt)
  pos=download(pos)
},
times = 10
)
ind=seq(1,n,by=stride)
B_dev=download(B_dev)
B=as.matrix(B_dev)
range(A[ind]-B[ind])





n=10240
type="double"
A=runif(n)
A_dev=gpuMatrix(A,type=type)
fileName="inst/script/performance test code.c"
opt=kernel.getOption()
#opt$kernelMsg$compilation.msg=T
tic()
for(i in 1:1000){
A_dev=gpuMatrix(A,type=type)
.kernel(file = fileName,kernel="concurrent",.globalThreadNum = 1,
          parms=list(A_dev,length(A_dev)),.options = opt)
A1=download(A_dev)
}
toc()


range(A*A-as.vector(A1))



