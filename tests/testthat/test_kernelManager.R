context("The functionality of the kernel manager")
N_size=1000

test_that("vector add",{
  A=runif(N_size)
  B=runif(N_size)
  dev_C=gpuEmptMatrix(N_size,1)
  
  #fileName="tests/testthat/opencl_code.c"
  fileName<- 'opencl_code.c'
  option=kernel.getOption()
  option$kernelMsg$insufficient.thread.num.warning=F
  
  .kernel(file=fileName,kernel="vectorAdd",parms=list(A,B,dev_C),.options=option)
  
  dev_C=download(dev_C)
  C=as.vector(dev_C)
  
  expect_equal(sum(abs(C-A-B)),0)
})
