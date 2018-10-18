C_call_assign<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  stopInfo=T
  if(is.call(leftExp)){
    if(length(leftExp)==1){
      stop("Unsupported left expression: ",deparse(Exp))
    }else{
      if(leftExp[[1]]=="["){
        CXXleftExp=CXXSubset(varInfo,Exp[[3]])
        stopInfo=F
      }
      if(stopInfo)
        stop("Unsupported left expression: ",deparse(Exp))
    }
  }else{
    CXXleftExp=deparse(leftExp)
  }
  
  stopInfo=T
  if(is.call(rightExp)){
    if(length(rightExp)==1){
      stop("Unsupported right expression: ",deparse(Exp))
    }else{
      if(rightExp[[1]]=="["){
        CXXrightExp=CXXSubset(varInfo,Exp[[3]])
        stopInfo=F
      }
      if(
        switch(deparse(rightExp[[1]]),"+"=T,"-"=T,"*"=T,"/"=T,F)
        ){
        CXXrightExp=CXXarithmaticOP(varInfo,Exp[[3]])
        stopInfo=F
      }
      if(stopInfo)
        stop("Unsupported right expression: ",deparse(Exp))
    }
  }else{
    CXXrightExp=deparse(rightExp)
  }
  curCode=paste0(CXXleftExp,deparse(Exp[[1]]),CXXrightExp,";")
  curCode
}


CXXarithmaticOP<-function(varInfo,Exp){
  leftEle=CXXSubset(varInfo,Exp[[2]])
  rightEle=CXXSubset(varInfo,Exp[[3]])
  code=paste0(leftEle,deparse(Exp[[1]]),rightEle)
  code
}

CXXSubset<-function(varInfo,Exp){
  if(length(Exp)==1)
    return(deparse(Exp))
  if(Exp[[1]]=="["){
    if(length(Exp)==3)
      return(deparse(Exp))
    if(length(Exp==4)){
      var_data=getVarInfo(varInfo,Exp[[2]])
      size1=evalChar(var_data$size1)
      if(Exp[[3]]==""||Exp[[4]]=="")
        stop("Compilation error, please contact the author: ",deparse(Exp))
      ExpChar=paste0(Exp[[2]],"[",Exp[[3]],"+",Exp[[4]],"*",size1,"]")
      return(ExpChar)
    }
  }
  stop("The expression is not a subset function: ",deparse(Exp))
}