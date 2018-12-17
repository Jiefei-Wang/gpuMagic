library(tictoc)
getDeviceList()

fileName="inst/script/performance test code.c"
src=readChar(fileName,file.info(fileName)$size)

kernelName=c("test_scalar","test_vector")
typeList=c("float","double")
deviceList=c(0,1,3)
sampleScale=c(1,4)

record=c()
for(k in 1:length(deviceList)){
  setDevice(deviceList[k])
  for(i in 1:length(typeList)){
    for(j in 1:length(kernelName)){
      type=typeList[i]
      kernel=kernelName[j]
      message(k,i,j)
      sampleSize=1024*4
      repNum=100
      A=rep(5,sampleSize)
      A_dev=gpuMatrix(rep(5,4),type=type)
      A_dev1=gpuMatrix(A,type=type)
      A_dev2=gpuMatrix(A,type=type)
      
      
      .kernel(src=src,kernel=kernel,.globalThreadNum = 4/sampleScale[j],parms=list(A_dev,1))
      res=system.time({
        for(rep in 1:10){
          .kernel(src=src,kernel=kernel,.globalThreadNum = sampleSize/sampleScale[j],parms=list(A_dev1,repNum))
        }
        A_dev1=download(A_dev1)
        }
      )
      record=rbind(record,c(k,type,kernel,res[3]))
    }
  }
}
#range(A_dev1[]-A_dev2[])
#which((A_dev1[]-A_dev2[])!=0)
