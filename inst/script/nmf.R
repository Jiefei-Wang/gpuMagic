
library(NMF)


gpuNMF<-function(V,W,H,iterNum=2000){
n=nrow(V)
m=ncol(V)
r=ncol(W)
dimNRM=c(n,r,m)
dtype="float"
shared_n=2000
debug=matrix(0,1,1)

#H
w_local=kernel.getSharedMem(shared_n,dtype)
tmp_m1=kernel.getSharedMem(m,dtype)
tmp_r=kernel.getSharedMem(r,dtype)
#W
h_local=kernel.getSharedMem(m,dtype)


W_dev=gpuMatrix(W,dtype)
V_dev=gpuMatrix(V,dtype)
H_dev=gpuMatrix(H,dtype)
dim_dev=gpuMatrix(dimNRM,"int")
size_dev=gpuMatrix(shared_n,"int")
debug_dev=gpuMatrix(debug,dtype)

opt=kernel.getOption()
threadNum=128
groupNum=r
opt$localThreadNum=threadNum
globalThreadNum=groupNum*threadNum

file <- 'inst/script/nmfKernel.cpp'
code=readChar(file, file.info(file)$size)
parms_H=list(W_dev,V_dev,H_dev,dim_dev,w_local,tmp_m1,
             tmp_r,size_dev,debug_dev)

parms_W=list(W_dev,V_dev,H_dev,dim_dev,h_local,
             tmp_r,debug_dev)

for(i in 1:iterNum)
  gpuNMF_iter(parms_H,parms_W,code,kernel,globalThreadNum,opt)


H_dev=download(H_dev)
res_H=as.matrix(H_dev)
W_dev=download(W_dev)
res_W=as.matrix(W_dev)

list(V=V,W=res_W,H=res_H)
}


gpuNMF_iter<-function(parms_H,parms_W,code,kernel,globalThreadNum,opt){
kernel="MatMul1"
.kernel(src=code,kernel=kernel,parms=parms_H,globalThreadNum=globalThreadNum,.options = opt)
kernel="MatMul2"
.kernel(src=code,kernel=kernel,parms=parms_W,globalThreadNum=globalThreadNum,.options = opt)
}



fit_nmf<-function(data,rank,seed="nndsvd",iter=1){
  nmf.options('maxIter'=iter)
  res=suppressWarnings(nmf(data,rank=rank,seed=seed))
  fittedRes=fit(res)
  W=fittedRes@W
  H=fittedRes@H
  return(list(W=W,H=H,V=data))
}

n=20000
r=50
m=200


W=matrix(runif(r*n),n,r)*10
H=matrix(runif(r*m),r,m)
V=W%*%H+matrix(runif(n*m),n,m)

seed=fit_nmf(V,r)
tic()
res=gpuNMF(V,seed$W,seed$H,2000)
toc()

range(V-seed$W%*%seed$H)
range(V-res$W%*%res$H)



tic()
res2=fit_nmf(V,r,iter=2000)
toc()

range(V-res2$W%*%res2$H)


