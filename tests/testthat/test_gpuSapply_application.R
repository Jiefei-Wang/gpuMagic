context("sapply application")

m=100
n=200
test_that("largest K value",{
largestK<-function(ind,A,k){
  #get a column of A
  A_col=subRef(A,,ind)
  #the largest k values and indices of A_col
  largest_value=matrix(0,k)
  largest_ind=matrix(0,k)
  #loop over A_col to find the largest k values
  for(i in 1:length(A_col)){
    for(j in 1:k){
      if(A_col[i]>largest_value[j]){
        if(j!=k){
          ind_src=seq(k-1,j,-1)
          ind_target=seq(k,j+1,-1)
          largest_value[ind_target]=largest_value[ind_src]
          largest_ind[ind_target]=largest_ind[ind_src]
        }
        largest_value[j]=A_col[i]
        largest_ind[j]=i
        break
      }
    }
  }
  return.nocpy(largest_ind)
}

k=10
A=matrix(runif(n*m),n,m)
res_gpu=gpuSapply(1:m,largestK,A=A,k=k,.macroParms = "k")
res_cpu=sapply(1:m,largestK,A=A,k=k)
#Check the error
expect_equal(sum(abs(res_gpu-res_cpu)),0)
})




test_that("Brute-force search",{
computeLoss<-function(ind,x,y,parms){
  #Find the parameters for the thread
  parm=parms[ind,]
  #Compute y hat, use no-copy transpose
  y_hat=x%*%t.nocpy(parm)
  #absolute loss value(L1 loss)
  loss=sum(abs(y-y_hat))
  return(loss)
}
#Sample size
n=500
#Number of parameters
p=2

beta=c(1,2)
#Generate data
x=matrix(runif(n*p),n,p)
e=runif(n)
y=x%*%beta+e

#brute-force search
search_range=seq(0,5,0.1)
parms=expand.grid(search_range,search_range)

res_gpu=gpuSapply(seq_len(nrow(parms)),computeLoss,x=x,y=y,parms=parms)
res_cpu=sapply(seq_len(nrow(parms)),computeLoss,x=x,y=y,parms=parms)

#print out the result
expect_equal(parms[which.min(res_gpu),],parms[which.min(res_cpu),])
})