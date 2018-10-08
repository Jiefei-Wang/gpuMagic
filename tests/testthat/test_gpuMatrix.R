

test_that("Matrix upload and download",{
  for(i in 1:5){
  k=100
  mydata=1:k
  mydata1=gpuMatrix(mydata,i)
  mydata1@data=0
  mydata1=download(mydata1)
  expect_equal(mydata1@data,1:k)
  }
})
