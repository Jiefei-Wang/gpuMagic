library("microbenchmark")
getDeviceList()
#setDevice(2)
.gpuResourcesManager$setMaxMemLimit(7*10^8)

testFunc_A<-function(ind,A,B){
  tmp=A[ind,]
  C=tmp%*%B
  return(C)
}
testFunc_A_subRef<-function(ind,A,B){
  tmp=subRef(A,ind,)
  C=tmp%*%B
  return(C)
}

testFunc_B<-function(ind,A,B){
  tmp=B[,ind]
  C=A%*%tmp
  return(C)
}
testFunc_B_subRef<-function(ind,A,B){
  tmp=subRef(B,,ind)
  C=A%*%tmp
  return(C)
}
n=1024
m=10000
k=1024

A=matrix(runif(n*m),n,m)
B=matrix(runif(m*k),m,k)

my_check<-function(values){
  values[[1]]=t(values[[1]])
  values[[2]]=t(values[[2]])
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

opt=gpuSapply.getOption()
#opt$kernelOption$localThreadNum=128
gpuMagic.option$setDefaultFloat("float")
microbenchmark(
  gpuSapply(1:n,testFunc_A,A,B,.options = opt),
  gpuSapply(1:n,testFunc_A_subRef,A,B,.options = opt),
  gpuSapply(1:k,testFunc_B,A,B,.options = opt),
  gpuSapply(1:k,testFunc_B_subRef,A,B,.options = opt),
  #A%*%B,
  #check=my_check,
  times = 10,
  control=list(warmup=1))

