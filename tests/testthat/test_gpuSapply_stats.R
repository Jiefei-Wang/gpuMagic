context("sapply statistics function")

n=100
m=200



test_that("gpuSapply: sum",{
  testFunc<-function(ind,A){
    tmp=sum(A[,ind]+A[,ind]*A[,ind])
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: colSums",{
  testFunc<-function(ind,A){
    tmp=colSums(A)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1,testFunc,A)
  res_cpu=sapply(1,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: rowSums",{
  testFunc<-function(ind,A){
    tmp=rowSums(A)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1,testFunc,A)
  res_cpu=sapply(1,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

