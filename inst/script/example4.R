library(tictoc)

testFunc2<-function(col_ind,A){
  largest=0
  largest_ind=1
  second_largest=0
  second_largest_ind=1
  for(i in 1:nrow(A)){
    curA=A[i,col_ind]
    if(curA>largest){
      second_largest=largest
      second_largest_ind=largest_ind
      largest=curA
      largest_ind=i
    }else{
      if(curA>second_largest){
        second_largest=curA
        second_largest_ind=i
      }
    }
   
  }
  return(second_largest_ind)
}
n=10000
m=10000
A=matrix(runif(max=1000,n*m),n,m)*10

#setDevice(3)
#gpuMagic.option$setDefaultFloat("float")
tic()
res_gpu=gpuSapply(1:m,testFunc2,A)
toc()
tic()
res_cpu=sapply(1:m,testFunc2,A)
toc()
tic()
A_rank=apply(-A,2,rank)
second_largest_ind=which(A_rank==2,arr.ind = T)[,1]
toc()

range(res_cpu-res_gpu)





