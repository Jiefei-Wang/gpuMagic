#These function does not have any meaning, just for making sure the GPU code can also be ran on CPU
gNumber<-function(precision=T_DEFAULT_float,p_static="N",t_static="N"){
  return(0)
}
gMatrix<-function(nrow=1,ncol=1,precision=T_DEFAULT_float,p_static="N",t_static="N",location="global"){
  return(matrix(NA,1,1))
}
resize<-function(data,nrow,ncol){
  return(matrix(data,nrow,ncol))
}


#Exp=parse(text="gMatrix()")[[1]]
#matchFunArg(gMatrix,Exp)
#Exp=parse(text="gNumber()")[[1]]