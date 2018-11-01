
test_that("Kernel run",{
  skip("Kernel run")
  for(i in 1:5){
    mydata1=1:10
    dev_data1=gpuMatrix(mydata1,i)
    mydata2=11:20
    dev_data2=gpuMatrix(mydata2,i)
    mydata3=rep(0,10)
    dev_data3=gpuMatrix(mydata3,i)
    fileName <- 'inst/script/kernelTest.cl'
    .kernel(file=fileName,kernel="vector_add",dev_data1,dev_data2,dev_data3)
    dev_data3=sync(dev_data3)
    expect_equal(as.vector(dev_data3),mydata1+mydata2)
  }
})








