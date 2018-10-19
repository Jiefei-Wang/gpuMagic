C_call_assign<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  if(is.call(leftExp)){
    if(length(leftExp)==1){
      stop("Unsupported left expression: ",deparse(Exp))
    }else{
      if(leftExp[[1]]=="["){
        C_leftExp=C_subset(varInfo,leftExp)
        stopInfo=F
      }
      if(stopInfo)
        stop("Unsupported left expression: ",deparse(Exp))
    }
  }else{
    C_leftExp=deparse(leftExp)
  }
  
  if(is.call(rightExp)){
    if(length(rightExp)==1){
      stop("Unsupported right expression: ",deparse(Exp))
    }else{
      if(rightExp[[1]]=="["){
        C_rightExp=C_subset(varInfo,rightExp)
        curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
        return(curCode)
      }
      if(
        switch(deparse(rightExp[[1]]),"+"=T,"-"=T,"*"=T,"/"=T,F)
        ){
        C_rightExp=C_arithmaticOP(varInfo,rightExp)
        curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
        return(curCode)
      }
      
      if(rightExp[[1]]=="matrix"){
        warning("matrix initial value will be ignored, but will be supported in future",deparse(Exp))
        #curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
        return("")
      }
      if(rightExp[[1]]=="nrow"||rightExp[[1]]=="ncol"||rightExp[[1]]=="length"){
        C_rightExp=C_length(varInfo,rightExp)
        curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
        return(curCode)
      }
        
      if(rightExp[[1]]=="floor"){
        C_rightExp=C_floor(varInfo,rightExp)
        curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
        return(curCode)
      }
      stop("Unsupported right expression: ",deparse(Exp))
    }
  }else{
    C_rightExp=deparse(rightExp)
    curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
  }
  
}

C_arithmaticOP<-function(varInfo,Exp){
  leftEle=C_subset(varInfo,Exp[[2]])
  rightEle=C_subset(varInfo,Exp[[3]])
  code=paste0(leftEle,deparse(Exp[[1]]),rightEle)
  code
}

C_length<-function(varInfo,Exp){
  if(Exp[[1]]=="nrow") return(R_getVarSize1(varInfo,Exp[[2]]))
  if(Exp[[1]]=="ncol") return(R_getVarSize2(varInfo,Exp[[2]]))
  if(Exp[[1]]=="length") return(paste0(R_getVarSize1(varInfo,Exp[[2]]),"*",R_getVarSize2(varInfo,Exp[[2]])))
}

C_subset<-function(varInfo,Exp){
  if(length(Exp)==1)
    return(deparse(Exp))
  if(Exp[[1]]=="["){
    if(length(Exp)==3){
      Exp[[3]]= parse(text=paste0(Exp[[3]],"-",1))[[1]]
      return(deparse(Exp))
    }
    if(length(Exp==4)){
      #var_data=getVarInfo(varInfo,Exp[[2]])
      #var_ind=varInfo[[Exp[[2]]]]
      size1=R_getVarSize1(varInfo,Exp[[2]])
      if(Exp[[3]]==""||Exp[[4]]=="")
        stop("Compilation error, please contact the author: ",deparse(Exp))
      ExpChar=paste0(Exp[[2]],"[",Exp[[3]],"-1 +(",Exp[[4]],"-1)*",size1,"]")
      return(ExpChar)
    }
  }
  stop("The expression is not a subset function: ",deparse(Exp))
}
C_floor<-function(varInfo,Exp){
  code=paste0("((int)",deparse(Exp),")")
  return(code)
}



R_getVarSize1<-function(varInfo,var){
  var_ind=varInfo$varTable[[deparse(var)]]-1
  size1=paste0(varInfo$GPUVar$gpu_matrix_size1,"[",var_ind,"]")
  size1
}
R_getVarSize2<-function(varInfo,var){
  var_ind=varInfo$varTable[[deparse(var)]]-1
  size2=paste0(varInfo$GPUVar$gpu_matrix_size2,"[",var_ind,"]")
  size2
}


