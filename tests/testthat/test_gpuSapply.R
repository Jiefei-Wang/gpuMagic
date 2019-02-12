context("Basic sapply function")

n=100
m=200

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
  expect_warning(res_gpu<-gpuSapply(1:m,testFunc,A))
  res_cpu=sapply(1:m,testFunc,A)
  error=range(res_gpu-res_cpu)
  expect_equal(sum(abs(error)),0)
})

