context("sapply matrix operation")

m=100
n=200

test_that("Matrix subset",{
  testFunc<-function(ind,A){
    tmp=A[,ind]
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("lazy reference",{
  testFunc<-function(ind,A){
    tmp=subRef(A,,ind)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Lazy reference, matrix subsetting and Arithmatic operation",{
  testFunc<-function(ind,A,B){
    tmp=subRef(B,,ind)
    tmp1=A[,ind]
    res=(tmp+tmp1)*tmp/tmp1
    return(res)
  }
  
  
  A=matrix(runif(n*m),n,m)+1
  B=matrix(runif(n*m),n,m)+1
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=(A+B)*B/A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Sequence add, positive step",{
  testFunc<-function(ind,n){
    tmp=1:n+1:n
    return(tmp)
  }
  res_gpu=gpuSapply(1,testFunc,n,.macroParms = "n")
  res_cpu=sapply(1,testFunc,n)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("Sequence add, negative step",{
  testFunc<-function(ind,n){
    tmp=n:1+n:1
    return(tmp)
  }
  res_gpu=gpuSapply(1,testFunc,n,.macroParms = "n")
  res_cpu=sapply(1,testFunc,n)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Sequence add, positve step, fix length",{
  testFunc<-function(ind,n,m){
    tmp=seq(1,n,length.out = m)+seq(1,n,length.out = m)
    return(tmp)
  }
  res_gpu=gpuSapply(1,testFunc,n,m,.macroParms = c("n","m"))
  res_cpu=sapply(1,testFunc,n,m)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})
test_that("Sequence add, negativ step, fix length",{
  testFunc<-function(ind,n,m){
    tmp=seq(n,1,length.out = m)+seq(n,1,length.out = m)
    return(tmp)
  }
  res_gpu=gpuSapply(1,testFunc,n,m,.macroParms = c("n","m"))
  res_cpu=sapply(1,testFunc,n,m)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("Sequence, no-creation method",{
  testFunc<-function(ind,A,B){
    tmp=A[1:4,ind]+B[1:4,ind]
    return(tmp)
  }
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=sapply(1:m,testFunc,A,B)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("Sequence, complicate no-creation method, sum function",{
  testFunc<-function(ind,A,B,end){
    tmp=sum(A[1:end[ind],ind]+B[1:end[ind],ind])
    return(tmp)
  }
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(n*m),n,m)
  end=sample(1:n,m,TRUE)
  res_gpu=gpuSapply(1:m,testFunc,A,B,end)
  res_cpu=sapply(1:m,testFunc,A,B,end)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Sequence, full version",{
  testFunc<-function(ind){
    tmp=seq(1,10,4)
    return(tmp)
  }
  res_gpu=gpuSapply(1:m,testFunc)
  res_cpu=sapply(1:m,testFunc)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Sequence, memory allocation",{
  testFunc<-function(ind){
    tmp=1:10
    tmp1=tmp
    return(tmp1)
  }
  res_gpu=gpuSapply(1:m,testFunc)
  res_cpu=sapply(1:m,testFunc)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Matrix add, transpose and no copy tranpose",{
  testFunc<-function(ind,A,B){
    tmp=t(A[ind,])
    tmp1=B[ind,]
    C=tmp+t_nocpy(tmp1)
    return_nocpy(C)
  }
  
  A=matrix(runif(n*m),m,n)
  B=matrix(runif(n*m),m,n)
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  expect_equal(res_gpu,t(A+B))
})

test_that("Matrix add, one argument is a scalar",{
  testFunc<-function(ind,A,B){
    tmp1=B[ind,]
    C=A+t_nocpy(tmp1)
    return_nocpy(C)
  }
  
  A=10
  B=matrix(runif(n*m),m,n)
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  expect_equal(res_gpu,t(A+B))
})

test_that("power function, one argument is a scalar",{
  testFunc<-function(ind,A,B){
    C=B[,ind]^A
    return_nocpy(C)
  }
  
  A=3
  B=matrix(runif(n*m),m,n)
  res_gpu=gpuSapply(1:n,testFunc,A,B)
  expect_equal(res_gpu,B^A)
})

test_that("abs function",{
  testFunc<-function(ind,A,B){
    C=abs(B[,ind])+A[,ind]
    return_nocpy(C)
  }
  
  A=matrix(runif(n*m),m,n)
  B=matrix(runif(n*m),m,n)-0.5
  res_gpu=gpuSapply(1:n,testFunc,A,B)
  expect_equal(res_gpu,abs(B)+A)
})
