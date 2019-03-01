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
  return_nocpy(largest_ind)
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
  y_hat=x%*%t_nocpy(parm)
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






test_that("T-test",{
t_stat=function(x,y){
  x.size=dim(x)
  y.size=dim(y)
  x.m=colMeans(x)
  y.m=colMeans(y)
  df=x.size[1]+y.size[1]-2
  s=(colSums(sweep(x, 2, x.m, `-`)^2)+colSums(sweep(y, 2, y.m, `-`)^2))/(df)
  
  t=abs((x.m-y.m)/sqrt(s*(1/x.size[1]+1/y.size[1])))
  t
}

t_stat_gpu=function(ind,data,sampleX,sampleY){
  x=subRef(data,sampleX[,ind],)
  y=subRef(data,sampleY[,ind],)
  x_row=nrow(x)
  y_row=nrow(y)
  x_m=t_nocpy(colSums(x))
  x_m=x_m/x_row
  y_m=t_nocpy(colSums(y))
  y_m=y_m/y_row
  df=x_row+y_row-2
  s=matrix(0,1,ncol(x))
  for(i in 1:x_row){
    s=s+(x[i,]-x_m)^2
  }
  for(i in 1:y_row){
    s=s+(y[i,]-y_m)^2
  }
  s=s/df
  
  t=abs((x_m-y_m)/(s*(1/x_row+1/y_row))^0.5)
  return(t)
}
t_stat_cpu<-function(ind,data,sampleX,sampleY){
  t_stat(data[sampleX[,ind],],data[sampleY[,ind],])
}

x_sampleN=50
y_sampleN=100
permute_num=10
total_sampleN=x_sampleN+y_sampleN
nParms=20
data=matrix(rnorm(total_sampleN*nParms),total_sampleN,nParms)

permute_ind=sapply(seq_len(permute_num),function(x)sample(1:total_sampleN))
permute_ind_x=permute_ind[1:x_sampleN,]
permute_ind_y=permute_ind[-(1:x_sampleN),]
res_gpu=gpuSapply(1:permute_num,t_stat_gpu,data,permute_ind_x,permute_ind_y)
res_cpu=sapply(1:permute_num,t_stat_cpu,data,permute_ind_x,permute_ind_y)
expect_equal(res_gpu,res_cpu)

})














