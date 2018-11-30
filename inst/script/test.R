a=matrix(1:9,3,3)
a_dev=gpuMatrix(a)
a_dev[1]<-10
a_dev[1,]<-10
a_dev[,1]<-10

a[1]
a[1,]
a[,1]


a[]<-10

b=as.list(a[1,])


"["<-function(...){
  match.call()
}
