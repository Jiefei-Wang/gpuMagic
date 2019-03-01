context("Basic sapply function")

n=100
m=200

test_that("macro1",{
  testFunc<-function(ind,A,n){
    B=matrix(ind,n)
    C=A+B
    return(C)
  }
  n=10
  A=matrix(runif(n),n)
  res_gpu=gpuSapply(1:m,testFunc,A,n,.macroParms = c("n"))
  res_cpu=sapply(1:m,testFunc,A,n)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

test_that("macro2",{
  testFunc<-function(ind,n){
    n=3
  }
  n=10
  expect_error(gpuSapply(1:m,testFunc,n,.macroParms = c("n")))
})
test_that("empty function",{
  testFunc<-function(ind){
    
  }
  
  expect_equal(gpuSapply(1:m,testFunc),rep(0,m))
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
  
  
  A=matrix(runif(n*m),n,m)
  res_gpu=gpuSapply(1:m,testFunc,A)
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})


test_that("Self assignment, Unnecessary bracket",{
  testFunc<-function(ind,A){
    {{
    A[,ind]=A[,ind]+1
    }}
    return(A[,ind])
  }
  
  
  A=matrix(runif(n*m),n,m)
  expect_message(res_gpu<-gpuSapply(1:m,testFunc,A))
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

