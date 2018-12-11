context("Basic sapply function")

N_size=100
M_size=200

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

#========================for loop====================
test_that("for loop iteration",{
  testFunc<-function(ind,A){
    res=0
    for(i in 1:nrow(A)){
      res=res+A[i,ind]
    }
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=colSums(A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("for loop control code, if condition",{
  testFunc<-function(ind,A){
    res=0
    for(i in 1:nrow(A)){
      if(A[i,ind]>0.5)
        next
      res=res+A[i,ind]
    }
    return(res)
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Unnecessary bracket",{
  testFunc<-function(ind,A){
    {{
    A[,ind]=A[,ind]+1
    }}
    return(A[,ind])
  }
  n=N_size
  m=M_size
  A=matrix(runif(n*m),n,m)
  expect_warning(res_gpu<-gpuSapply(1:m,testFunc,A))
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})



