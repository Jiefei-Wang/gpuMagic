#These function does not have any meaning, just for making sure the GPU code can also be ran on CPU
gNumber<-function(precision=gpuMagic.option$getDefaultFloat(),constDef=FALSE){
  return(0)
}
gMatrix<-function(nrow=1,ncol=1,precision=gpuMagic.option$getDefaultFloat(),constDef=FALSE,shared=FALSE,location="global"){
  return(matrix(NA,nrow,ncol))
}
resize<-function(data,nrow,ncol){
  return(matrix(data,nrow,ncol))
}
subRef<-function(variable,i="",j=""){
  if(i==""&&j=="")
    return(variable[,,drop=F])
  if(i=="")
    return(variable[,j,drop=F])
  if(j=="")
    return(variable[i,,drop=F])
    return(variable[i,j,drop=F])
}



#Exp=parse(text="gMatrix()")[[1]]
#matchFunArg(gMatrix,Exp)
#Exp=parse(text="gNumber()")[[1]]