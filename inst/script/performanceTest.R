library(tictoc)
tic()
for(i in 1:2000){
  .kernel(file=file,kernel=kernel,parms=parms,globalThreadNum=groupNum*threadNum,.options = opt)
}
debug_dev=download(debug_dev)
toc()
n*m*r+n*r*r+r*m*r+2*n*r


tic()
for(i in 1:2000){
  .kernel(file=file,kernel=kernel,parms=parms,globalThreadNum=groupNum*threadNum,.options = opt)
  debug_dev=download(debug_dev)
}
toc()

n*m*r+n*r*r+r*m*r+2*n*r