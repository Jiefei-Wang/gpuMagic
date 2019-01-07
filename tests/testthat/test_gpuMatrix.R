context("gpuMatrix operation")

test_that("Matrix upload and download",{
  
  type=c("bool","char","half","float","double","int","long","uint","ulong")
  for(i in 1:8){
    k=100
    mydata=1:k
    mydata1=gpuMatrix(mydata,type[i])
    mydata1[]=0
    mydata1=download(mydata1)
  expect_equal(as.vector(mydata1),1:k)
  }
})



#Cannot pass now
test_that("Matrix assignment",{
  type=c("char","half","float","double","int","long","uint","ulong")
  for(i in 1:5){
    k=100
    mydata=matrix(1:k,10)
    mydata_dev=gpuMatrix(mydata,type[i])

    mydata_dev[1]=10
    mydata_dev[2,]=11
    mydata_dev[,3]=12
    mydata_dev[4,4]=15
    
    mydata[1]=10
    mydata[2,]=11
    mydata[,3]=12
    mydata[4,4]=15

    expect_equal(as.matrix(mydata_dev),mydata)
  }
})
