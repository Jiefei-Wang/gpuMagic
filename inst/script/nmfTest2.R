
n=20000
r=50
m=200
dtype="float"

W=matrix(runif(r*n),n,r)*10
H=matrix(runif(r*m),r,m)
V=W%*%H
dimNRM=c(n,r,m)
debug=matrix(0,n,r)

h_local=kernel.getSharedMem(m,dtype)
tmp_r=kernel.getSharedMem(r,dtype)





W_dev=gpuMatrix(W,dtype)
V_dev=gpuMatrix(V,dtype)
H_dev=gpuMatrix(H,dtype)
dim_dev=gpuMatrix(dimNRM,"int")
debug_dev=gpuMatrix(debug,dtype)

# void MatMul2(gATUO* W,gATUO *V, gATUO* H,
#              global int* dimNRM,
#              lATUO* h_local,lATUO * tmp_r,
#              gATUO* debug)

file <- 'inst/script/nmfKernel.cpp'
kernel="MatMul2"
parms=list(W_dev,V_dev,H_dev,dim_dev,h_local,
           tmp_r,debug_dev)
opt=kernel.getOption()
threadNum=128
groupNum=r
opt$localThreadNum=threadNum
opt$verbose=F


.kernel(src=code,kernel=kernel,parms=parms,globalThreadNum=groupNum*threadNum,.options = opt)


W_dev=download(W_dev)
res=as.matrix(W_dev)
#res1=V%*%t(H)
#res1=H%*%t(H)
res1=W*(V%*%t(H))/(W%*%H%*%t(H))
error=res-res1
range(error)



