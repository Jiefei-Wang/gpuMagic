context("sapply statistics function")


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


test_that("Matrix subsetting, Arithmatic operation",{
  testFunc<-function(ind,A,B,C){
    res=B[,ind]*A[,ind]/C[,ind]+A[,ind]-B[,ind]
    return(res)
  }
  
  
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(n*m),n,m)
  C=matrix(runif(n*m),n,m)
  
  res_gpu=gpuSapply(1:m,testFunc,A,B,C)
  res_cpu=sapply(1:m,testFunc,A,B,C)
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