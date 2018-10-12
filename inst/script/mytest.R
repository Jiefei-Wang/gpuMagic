
#dyn.load(.parms$getLibPath())
#dyn.unload(.parms$getLibPath())
#detach("package:openSparse",unload = T)



mydata=1:10
mydata1=gpuMatrix(mydata,1)
mydata1@data=0
mydata1=download(mydata1)


convertDataType(mydata,T_F64)
.gpuResourcesManager$getGPUusage()
.gpuResourcesManager$releaseAll()





mydata1=rep(0,10)
dev_data1=gpuMatrix(mydata1)
mydata2=11:20
dev_data2=gpuMatrix(mydata2)
mydata3=rep(0,10)
dev_data3=gpuMatrix(mydata3)


fileName <- 'inst/script/kernelTest.cl'



.kernel(file=fileName,kernel="vector_add",mydata1,mydata2,dev_data3)
dev_data3=sync(dev_data3)

codePack=readCode(fileName,"")
parms=list(A,B,dev_C,dim(A),dim(B),dim(C))
parseProgram(codePack,kernel="matrix_product",parms)$src




library("tictoc")
fileName <- 'inst/script/kernelMatrix.cl'

threadNum=64
n=threadNum*16*2

A=matrix(runif(n*n),n,n)
B=matrix(runif(n*n),n,n)
C=matrix(0,n,n)
#Allocate GPU memory
dev_C=gpuMatrix(C,T_F64)

##GPU computing
tic()
.kernel(file=fileName,kernel="matrix_product",A,B,dev_C,dim(A),dim(B),dim(C),threadNum = threadNum)
dev_C=sync(dev_C)
toc()

##R internel function
tic()
expectRes=A%*%B
toc()

#The differences between GPU and R
error=sum(abs(as.matrix(dev_C)-expectRes))
error

.gpuResourcesManager$getGPUusage()

