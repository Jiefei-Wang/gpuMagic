context("Basic sapply function")

N_size=1000
M_size=2000

test_that("Matrix subset",{
  testFunc<-function(ind,A){
    tmp=A[,ind]
    return(tmp)
  }
  n=N_size
  m=M_size
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
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Matrix subsetting, Arithmatic operation *",{
  testFunc<-function(ind,A,B){
    tmp=B[,ind]
    tmp1=A[,ind]
    res=tmp*tmp1
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=B*A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("Matrix subsetting, Arithmatic operation /",{
  testFunc<-function(ind,A,B){
    tmp=B[,ind]
    tmp1=A[,ind]
    res=tmp/tmp1
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)+1
  B=matrix(runif(n*m),n,m)+1
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=B/A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("Lazy reference, Arithmatic operation *",{
  testFunc<-function(ind,A,B){
    tmp=subRef(B,,ind)
    tmp1=subRef(A,,ind)
    res=tmp*tmp1
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=B*A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Lazy reference, Arithmatic operation /",{
  testFunc<-function(ind,A,B){
    tmp=subRef(B,,ind)
    tmp1=subRef(A,,ind)
    res=tmp/tmp1
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)+1
  B=matrix(runif(n*m),n,m)+1
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=B/A
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
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)+1
  B=matrix(runif(n*m),n,m)+1
  res_gpu=gpuSapply(1:m,testFunc,A,B)
  res_cpu=(A+B)*B/A
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})




