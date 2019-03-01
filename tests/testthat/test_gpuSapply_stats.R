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



test_that("gpuSapply: rowMeans",{
  testFunc<-function(ind,A){
    tmp=rowMeans(A)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1,testFunc,A)
  res_cpu=sapply(1,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: colMeans",{
  testFunc<-function(ind,A){
    tmp=colMeans(A)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1,testFunc,A)
  res_cpu=sapply(1,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: sweep1 character function",{
  testFunc<-function(ind,A,b){
    tmp=sweep(A+5,1,b,'*')
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  b=1:n
  res_gpu=gpuSapply(1,testFunc,A,b)
  expect_error(gpuSapply(1,testFunc,A,t(b)))
  res_cpu=sapply(1,testFunc,A,b)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: sweep2 character function",{
  testFunc<-function(ind,A,b){
    tmp=sweep(A+5,2,b,'*')
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  b=t(1:m)
  res_gpu=gpuSapply(1,testFunc,A,b)
  expect_error(gpuSapply(1,testFunc,A,t(b)))
  res_cpu=sapply(1,testFunc,A,b)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("gpuSapply: sweep1 symbolic function",{
  testFunc<-function(ind,A,b){
    tmp=sweep(A+5,1,b,`*`)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  b=1:n
  res_gpu=gpuSapply(1,testFunc,A,b)
  expect_error(gpuSapply(1,testFunc,A,t(b)))
  res_cpu=sapply(1,testFunc,A,b)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("gpuSapply: sweep2 symbolic function",{
  testFunc<-function(ind,A,b){
    tmp=sweep(A+5,2,b,`*`)
    return(tmp)
  }
  
  
  A=matrix(runif(n*m),n,m)
  b=t(1:m)
  res_gpu=gpuSapply(1,testFunc,A,b)
  expect_error(gpuSapply(1,testFunc,A,t(b)))
  res_cpu=sapply(1,testFunc,A,b)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})
