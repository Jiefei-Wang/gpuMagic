context("A smart man will never fall at the same place twice.")

n=100
m=200
k=50

test_that("matrix multiplication",{
  testFunc<-function(ind,A,B){
    C=matrix(0,nrow(A),1)
    for(i in 1:nrow(A)){
      tmp=0
      for(k in 1:ncol(A)){
        tmp=tmp+A[i,k]*B[k,ind]
      }
      C[i]=tmp
    }
    return(C)
  }
  
  
  A=matrix(runif(n*m),n,m)
  B=matrix(runif(k*m),m,k)
  res_gpu=gpuSapply(1:k,testFunc,A,B)
  res_cpu=sapply(1:k,testFunc,A,B)
  
  expect_equal(res_gpu,res_cpu)
})


