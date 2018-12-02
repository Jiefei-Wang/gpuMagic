n=20000
A=runif(n)
B=runif(n)
dev_C=gpuEmptMatrix(n,1)
fileName<- 'inst/script/c_code.R'

.kernel(file=fileName,kernel="vectorAdd",parms=list(A,B,dev_C))


dev_C=download(dev_C)

C=as.matrix(dev_C)

range(C-A-B)
