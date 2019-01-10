
#===========================profiler 1========================

#Profile a parameter and give the profile table back
profileVar<-function(parms,macroParms){
  varInfo=getEmpVarInfoTbl()
  varInfo$parmsTblName="parms"
  varInfo$requiredVar=c()
  
  varName=names(parms)
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="gpuMatrix"){
      curPrecision=.type(parms[[i]])
      curDim=dim(parms[[i]])
    }else{
      curPrecision=GPUVar$default_float
      curDim=dim(as.matrix(parms[[i]]))
    }
    info=getEmpyTable()
    info$var=varName[i]
    
    
    info$precisionType=curPrecision
    info$shared=TRUE
    info$constVal=varName[i]%in%macroParms
    info$compileValue=info$constVal
    info$compileSize1=TRUE
    info$compileSize2=TRUE
    info$require=TRUE
    info$initialization=FALSE
    
    
    if(info$compileValue){
      info$value=paste0("(",varInfo$parmsTblName,"[[",i,"]])")
    }
    
    if(curDim[1]==1&&curDim[2]==1&&varName[i]!=GPUVar$gpu_loop_data){
      info$dataType=T_scale
      info$size1=1
      info$size2=1
    }else{
      info$dataType=T_matrix
      if(varName[i]==GPUVar$gpu_loop_data){
        info$size1=paste0("length(",varInfo$parmsTblName,"[[",i,"]])")
        info$size2=1
      }else{
        info$size1=paste0("nrow(",varInfo$parmsTblName,"[[",i,"]])")
        info$size2=paste0("ncol(",varInfo$parmsTblName,"[[",i,"]])")
      }
    }
    
    
    varInfo=addVarInfo(varInfo,info)
    varInfo$requiredVar=c(varInfo$requiredVar,info$var)
  }
  varInfo
}

#==================================Profiler 2==========================

#Find the function parameters
#If the functions' argument does not show in the expression, the default value will be used
matchFunArg<-function(fun,Exp){
  funArg=formals(fun)
  eval(parse(text=paste0(deparse(Exp[[1]]),"=fun")))
  ExpArg=standardise_call(Exp)
  if(length(ExpArg)>1){
    argName=names(ExpArg)
    for(i in 2:length(ExpArg)){
      funArg[[argName[i]]]=ExpArg[[i]]
    }
  }
  for(i in 1:length(funArg)){
    if(deparse(funArg[[i]])=="")
      funArg[[i]]=NA
    if(is.call(funArg[[i]]))
      funArg[[i]]=eval(funArg[[i]])
  }
  return(funArg)
}
#Get the right expression profile
getExpInfo<-function(varInfo,Exp,errorCheck=FALSE){
  res=getExpInfo_hidden(varInfo,Exp)
  if(is.data.frame(res)){
    ExpInfo=res
  }else{
    ExpInfo=res$ExpInfo
    if(errorCheck&&!is.null(res[["errorCheck"]])){
      checkNum=varInfo$errorCheck[[".checkNumber"]]+1
      varInfo$errorCheck[[as.character(checkNum)]]=res$errorCheck
    }
  }
  
  
  #If the variable is explicit definition
  if(is.call(Exp)&&(deparse(Exp[[1]])%in% .profileExplicitDefine))
    return(ExpInfo)
  #Some optimization
  if(ExpInfo$compileSize1&&ExpInfo$compileSize2&&
     Simplify(ExpInfo$size1)=="1"&&Simplify(ExpInfo$size2)=="1"){
    ExpInfo$dataType=T_scale
    ExpInfo$size1=1
    ExpInfo$size2=1
  }
  if(ExpInfo$dataType==T_scale)
    ExpInfo$location="local"
  
  return(ExpInfo)
}
getExpInfo_hidden<-function(varInfo,Exp){
  if(isNumeric(Exp)){
    ExpInfo=profile_numeric(Exp)
    return(ExpInfo)
  }
  #If the expression is a function call
  if(is.call(Exp)){
    func=deparse(Exp[[1]])
    if(!is.null(.profileFuncs[[func]])){
      ExpInfo=.profileFuncs[[func]](varInfo,Exp)
      return(ExpInfo)
    }
    stop("Unsupported function: ",deparse(Exp))
  }
  
  #If not the above case, the expression will be treated as a variable
  if(is.symbol(Exp)){
    ExpInfo=profile_symbol(varInfo,Exp)
    return(ExpInfo)
  }
  
  
  stop("Unknow code: ",deparse(Exp))
}


checkVarType<-function(leftInfo,rightInfo){
  if(leftInfo$dataType!=rightInfo$dataType)
    return(list(needReassign=TRUE))
  if(leftInfo$size1!=rightInfo$size1||rightInfo$size2!=rightInfo$size2){
    return(list(needReassign=TRUE))
  }
  if(typeInherit(leftInfo$precisionType,rightInfo$precisionType)!=leftInfo$precisionType)
    return(list(needReassign=FALSE,needRetype=TRUE,valueUpdate=TRUE))
  if(leftInfo$value!=rightInfo$value||leftInfo$compileValue!=rightInfo$compileValue)
    return(list(needReassign=FALSE,needRetype=FALSE,valueUpdate=TRUE))
  return(list(needReassign=FALSE,needRetype=FALSE,valueUpdate=FALSE))
}

#Determine which type can preserve the information 
#of the information in type1 and type2
typeInherit<-function(type1,type2){
  if(!is.character(type1))
    type1=as.character(type1)
  if(!is.character(type2))
    type2=as.character(type2)
  
  group_float=c("half","float","double")
  group_int=c("bool","char","int","long","uint","ulong")
  
  target_size=max(getTypeSize(type1),getTypeSize(type2))
  if(type1 %in% group_float||type2%in% group_float){
    for(i in 1:length(group_float)){
      if(target_size==getTypeSize(group_float[i]))
        return(group_float[i])
    }
  }
  if(type1 %in% group_int||type2%in% group_int){
    for(i in 1:length(group_int)){
      if(target_size==getTypeSize(group_int[i]))
        return(group_int[i])
    }
  }
  for(i in 1:length(group_float)){
    if(target_size==getTypeSize(group_float[i]))
      return(group_int[i])
  }
  stop("Unsupported variable type!")
}

is.preservedFunc<-function(func){
  func=as.character(func)
  length(grep(GPUVar$preservedFuncPrefix,func,fixed = T))!=0
}
#This function determine when the variable is defined, which property can be inherit from the right expression
copyVarInfo<-function(info,fullCopy=FALSE){
  
  info$version=1
  if(fullCopy)
    return(info)
  
  info$shared=FALSE
  
  info$require=FALSE
  info$constVal=FALSE
  info$constDef=FALSE
  info$isRef=FALSE
  info$initialization=TRUE
  info$changed=FALSE
  info$used=FALSE
  
  
  info
}
#Format a single code
formatCall<-function(Exp,generalType=FALSE){
  if(is.numeric(Exp)){
    if(generalType)
      return(as.symbol("gType"))
    else
      return(as.symbol("num"))
  }
  if(!is.call(Exp)){
    if(generalType)
      return(as.symbol("gType"))
    else
      return(as.symbol("var"))
  }
  if(length(Exp)>1){
    for(i in 2:length(Exp)){
      Exp[[i]]=formatCall(Exp[[i]],generalType)
    }
  }
  Exp
}
#Test if an input is a number
#x can be a character or an expression
isNumeric<-function(x){
  if(!is.call(x)&&length(x)>1)
    return(FALSE)
  
  xExp=NULL
  try({xExp=toExpression(x)},silent = T)
  if(is.null(xExp)) 
    return(FALSE)
  if(is.call(xExp)){
    if(xExp[[1]]!="-"&&xExp[[1]]!="+")
      return(FALSE)
    if(length(xExp)!=2)
      return(FALSE)
    else
      return(isNumeric(xExp[[2]]))
  }
  res=is.numeric(xExp)
  return(res)
}

toCharacter<-function(x){
  if(is.language(x)){
    var_char=deparse(x)
  }else{
    if(is.character(x))
      var_char=x
    else{
      var_char=as.character(x)
    }
  }
  var_char
}
#Convert an non-expression to the expression and return both
#expression and characters
toExpression<-function(var){
  if(is.language(var)){
    var_char=deparse(var)
  }else{
    if(is.character(var))
      var_char=var
    else{
      var_char=as.character(var)
    }
    var=parse(text=var_char)[[1]]
  }
  return(var)
}


#This function simplify the R code and make it ready to put in the varInfo table
Simplify2<-function(Exp){
  res=Simplify(Exp)
  #remove the space
  #res=trimws(gsub(", ",",",res,fixed = T))
  #If the result is a vector
  # if(length(grep(" ",res,fixed = T))!=0){
  #   res=paste0("c(",gsub(" +",",",res),")")
  #   return(res)
  # }
  if(isNumeric(res))
    return(res)
  else
    return(paste0("(",res,")"))
}
#get the version bump code
#var: the variable name
#version: the version that should be bumped to
getVersionBumpCode<-function(var,version){
  var_char=toCharacter(var)
  parse(text=paste0(GPUVar$preservedFuncPrefix,"setVersion(",var_char,",",version,")"))[[1]]
}
#Add the error check into the varInfo
#level: the error level: warning, error
#code: The code that generate this error check
#check: the condition that will throw the error(check=TRUE will throw the error)
#msg: the message that will be post when the error occurs
setErrorCheck<-function(level,code,check,msg=""){
 data.frame(level=level,code=code,check=check,msg=msg,stringsAsFactors=FALSE)
}

#Redirect the variable to an exist variable to save the memory space
redirectVar<-function(varInfo,sourceVar,desVar){
  sourceVarInfo=getVarInfo(varInfo,sourceVar)
  if(hasVar(varInfo,desVar)){
    desVarInfo=getVarInfo(varInfo,desVar)
    #If the destination is a lazy ref or seq object, no redirection is available
    if(desVarInfo$isRef||desVarInfo$isSeq){
      return()
    }
    #Check if the variable can be redirect
    if(curInfo$require||
       sourceVar$dataType!=desVar$dataType||
       sourceVar$shared!=desVar$shared||
       sourceVar$location!=desVar$location||
       desVar$precisionType!=typeInherit(sourceVar$precisionType,desVar$precisionType))
      return()
    if(desVarInfo$redirect=="NA"){
      sourceVarInfo$redirect=desVar
    }else{
      sourceVarInfo$redirect=desVarInfo$redirect
    }
    return(sourceVarInfo)
  }else{
    sourceVarInfo$redirect=desVar
    return(sourceVarInfo)
  }
}
