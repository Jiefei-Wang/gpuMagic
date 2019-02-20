#Extract the variable if the same function is nested in the function
extractVarIfFuncIsSame<-function(Exp,func){
  if(!is.call(Exp))
    Exp
  if(Exp[[1]]=="(")
    return(extractVarIfFuncIsSame(Exp[[2]],func))
  if(Exp[[1]]!=func)
    return(Exp)
  res=list()
  for(i in seq_len(length(Exp)-1)+1){
    res=c(res,extractVarIfFuncIsSame(Exp[[i]],func))
  }
  return(res)
}



#Exp=quote(max(nrow(parms[[3]]), nrow(parms[[2]])))
#Exp=quote(max((max(max(nrow(parms[[3]]), nrow(parms[[2]])), nrow(parms[[3]]))),nrow(parms[[2]])))
#Exp=quote(max(nrow(parms[[3]]), nrow(parms[[2]])))
Simplify_plus<-function(Exp){
  if(is.call(Exp)){
    func=Exp[[1]]
    Exp1=Simplify_plus(Exp[[2]])
    Exp1_char=deparse(Exp1)
    if(length(Exp)>2){
      Exp2=Simplify_plus(Exp[[3]])
      Exp2_char=deparse(Exp2)
    }
    if(func=="max"||func=="min"){
      if(Exp1_char==Exp2_char)
        return(Exp1)
      if(Exp1_char=="1"&&is.call(Exp2)&&deparse(Exp2[[1]])%in%c("length","nrow","ncol")){
        if(func=="max")
          return(Exp2)
        else
          return(1)
      }
      if(Exp2_char=="1"&&is.call(Exp1)&&deparse(Exp1[[1]])%in%c("length","nrow","ncol")){
        if(func=="max")
          return(Exp1)
        else
          return(1)
      }
      symbolList=extractVarIfFuncIsSame(Exp,func)
      if(length(symbolList)>2){
        symbolList_new=unique(symbolList)
        if(length(symbolList_new)!=length(symbolList)){
          code=paste0(deparse(func),"(",deparse(symbolList_new[[1]]),",",deparse(symbolList_new[[2]]),")")
          for(i in seq_len(length(symbolList_new)-2)+2){
            code=paste0(deparse(func),"(",code,",",deparse(symbolList_new[[i]]),")")
          }
          return(parse(text=code)[[1]])
        }else{
          return(Exp)
        }
      }
      
      
    }
    if(func=="||"){
      if(Exp1==TRUE||Exp2==TRUE)
        return(quote(TRUE))
      if(Exp1==FALSE)
        return(Exp2)
      if(Exp2==FALSE)
        return(Exp1)
    }
    if(func=="&&"){
      if(Exp1==FALSE||Exp2==FALSE)
        return(quote(FALSE))
      if(Exp1==TRUE)
        return(Exp2)
      if(Exp2==TRUE)
        return(Exp1)
    }
    if(func=="=="){
      if(Exp1_char==Exp2_char)
        return(TRUE)
    }
    if(func=="!"){
      if(is.logical(Exp1)){
        return(!Exp1)
      }
    }
    if(func=="("){
      if(is.call(Exp1)){
        funcGroup=getGroup(deparse(Exp1[[1]]))
        mathOP=length(funcGroup)!=0&&funcGroup[[1]]=="Arith"
      }else{
        mathOP=FALSE
      }
      if(length(Exp1)==1||!mathOP){
        return(Exp1)
      }
    }
    if(deparse(func)%in%c("!=",">","<")){
      if(Exp1_char==Exp2_char)
        return(FALSE)
    }
    for(i in seq_len(length(Exp)-1)+1){
      Exp[[i]]=Simplify_plus(Exp[[i]])
    }
    return(Exp)
  }
  
  return(Exp)
}
