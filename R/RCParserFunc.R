


C_call_assign<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  if(is.call(leftExp)){
    func_char=deparse(leftExp[[1]])
    func=.cFuncs[[func_char]]
    if(!is.null(func)){
      C_leftExp=func(varInfo,leftExp)
    }else{
      stop("Unsupported left expression: ",deparse(Exp))
    }
  }else{
    C_leftExp=deparse(leftExp)
  }
  
  if(is.call(rightExp)){
    func_char=deparse(rightExp[[1]])
    func=.cFuncs[[func_char]]
    if(!is.null(func)){
      C_rightExp=func(varInfo,rightExp)
    }else{
      stop("Unsupported right expression: ",deparse(Exp))
    }
    
  }else{
    C_rightExp=deparse(rightExp)
  }
  
  if(C_rightExp==""){
    if(deparse(Exp[[1]])=="==")
      stop("Unexpected expression: ",deparse(Exp))
    return("")
  }
  if(C_leftExp==""){
    stop("Unexpected expression: ",deparse(Exp))
  }
  
  
  curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
  return(curCode)
  
}

C_arithmaticOP<-function(varInfo,Exp){
  leftEle=C_subset(varInfo,Exp[[2]])
  rightEle=C_subset(varInfo,Exp[[3]])
  code=paste0(leftEle,deparse(Exp[[1]]),rightEle)
  code
}

C_length<-function(varInfo,Exp){
  return(paste0(R_getVarSize1(varInfo,Exp[[2]]),"*",R_getVarSize2(varInfo,Exp[[2]])))
}
C_nrow<-function(varInfo,Exp){
  return(R_getVarSize1(varInfo,Exp[[2]]))
}
C_ncol<-function(varInfo,Exp){
  return(R_getVarSize2(varInfo,Exp[[2]]))
}
C_subset<-function(varInfo,Exp){
  if(length(Exp)==1){
    curVar=deparse(Exp)
    if(is.numeric(Exp))
      return(curVar)
    curInfo=getVarInfo(varInfo,curVar,1)
    if(curInfo$dataType==T_matrix)
      return(paste0(curVar$address,"[0]"))
    if(curInfo$dataType==T_scale)
      return(paste0(curInfo$address))
  }
  if(Exp[[1]]=="["){
    if(length(Exp)==3){
      ExpChar=paste0(Exp[[2]],"[(unsigned int)",C_subset(varInfo,Exp[[3]]),"-1]")
      return(ExpChar)
    }
    if(length(Exp==4)){
      #var_data=getVarInfo(varInfo,Exp[[2]])
      #var_ind=varInfo[[Exp[[2]]]]
      size1=R_getVarSize1(varInfo,Exp[[2]])
      if(Exp[[3]]==""||Exp[[4]]=="")
        stop("Compilation error, please contact the author: ",deparse(Exp))
      ExpChar=paste0(Exp[[2]],"[(unsigned int)",C_subset(varInfo,Exp[[3]]),
                     "-1 +((unsigned int)",C_subset(varInfo,Exp[[4]]),"-1)*",size1,"]")
      return(ExpChar)
    }
  }
  stop("The expression is not a subset function: ",deparse(Exp))
}
C_floor<-function(varInfo,Exp){
  code=paste0("floor(",deparse(Exp[[2]]),")")
  return(code)
}

C_NULL<-function(varInfo,Exp){
  return("")
}





R_getVarSize<-function(varInfo,var,ind){
  curInfo=getVarInfo(varInfo,deparse(var),1)
  var_ind=varInfo$matrixInd[[deparse(var)]]
  loc=NA
  if(curInfo$location=="global"&&!curInfo$shared)
    loc="global_private_"
  if(curInfo$location=="global"&&curInfo$shared)
    loc="global_shared_"
  if(curInfo$location=="local"&&!curInfo$shared)
    loc="local_private_"
  if(curInfo$location=="local"&&curInfo$shared)
    loc="local_shared_"
  if(is.na(loc))
    stop("undetermined matrix property!")
  
    
  size=paste0(GPUVar[[paste0(loc,"size",ind)]],"[",var_ind,"]")
  
  size
}
R_getVarSize1<-function(varInfo,var){
  R_getVarSize(varInfo,var,1)
}
R_getVarSize2<-function(varInfo,var){
  R_getVarSize(varInfo,var,2)
}


