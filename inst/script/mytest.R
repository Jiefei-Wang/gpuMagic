
#dyn.load(.parms$getLibPath())
#dyn.unload(.parms$getLibPath())
#detach("package:openSparse",unload = T)



mydata=rep(1,10)
mydata1=gpuMatrix(mydata,"char")
mydata1[]=0
mydata1=download(mydata1)


convertDataType(mydata,"double")
.gpuResourcesManager$getGPUusage()
.gpuResourcesManager$releaseAll()





mydata1=1:10
dev_data1=gpuMatrix(mydata1)
mydata2=11:20
dev_data2=gpuMatrix(mydata2)
mydata3=rep(0,10)
dev_data3=gpuMatrix(mydata3)


fileName <- 'inst/script/kernelTest.cl'
.kernel(file=fileName,kernel="vector_add",
        parms = list(dev_data1,dev_data2,dev_data3))
dev_data3=sync(dev_data3)

codePack=readCode(fileName,"")
parms=list(A,B,dev_C,dim(A),dim(B),dim(C))
parseProgram(codePack,kernel="matrix_product",parms)$src









