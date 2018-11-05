
n=20000
r=50
m=200
shared_n=2000
dtype="float"

W=matrix(runif(r*n),n,r)*10
H=matrix(runif(r*m),r,m)
V=W%*%H
dimNRM=c(n,r,m)
size=c(shared_n)
debug=matrix(0,r,m)

w_local=kernel.getSharedMem(shared_n,dtype)
tmp_m1=kernel.getSharedMem(m,dtype)
tmp_r=kernel.getSharedMem(r,dtype)


W_dev=gpuMatrix(W,dtype)
V_dev=gpuMatrix(V,dtype)
H_dev=gpuMatrix(H,dtype)
dim_dev=gpuMatrix(dimNRM,"int")
size_dev=gpuMatrix(size,"int")
debug_dev=gpuMatrix(debug,dtype)

# kernel void MatMul1(gATUO* W,gATUO *V, gATUO* H,
#                     global int* dimNRM,
#                     lATUO* w_local, lATUO * tmp_m1, lATUO * tmp_m2,
#                     lATUO * tmp_r, global int* size,gATUO* debug)

file <- 'inst/script/nmfKernel.cpp'
kernel="MatMul1"
parms=list(W_dev,V_dev,H_dev,dim_dev,w_local,tmp_m1,
           tmp_r,size_dev,debug_dev)
opt=kernel.getOption()
threadNum=128
groupNum=r
opt$localThreadNum=threadNum
opt$verbose=F

.kernel(src=code,kernel=kernel,parms=parms,globalThreadNum=groupNum*threadNum,.options = opt)
H_dev=download(H_dev)
res=as.matrix(H_dev)

#debug_dev=download(debug_dev)
#res=as.matrix(debug_dev)
#res1=(t(W)%*%W%*%H)
#res1=(t(W)%*%V)
res1=H*(t(W)%*%V)/(t(W)%*%W%*%H)

error=res-res1
range(error)
#which(abs(error)==max(abs(error)),arr.ind=T)
