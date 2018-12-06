
#===========================profiler 1========================

#Profile a parameter and give the profile table back
profileVar<-function(parms,macroParms){
  varInfo=getEmpVarInfoTbl()
  varInfo$parmsTblName="parms"
  varInfo$requiredVar=c()
  
  if(length(parms)==0) return(varInfo)
  varName=names(parms)
  for(i in 1:length(parms)){
    if(class(parms[[i]])=="gpuMatrix"){
      curPrecision=.type(parms[[i]])
      curDim=dim(parms[[i]])
    }else{
      curPrecision=gpuMagic.option$getDefaultFloat()
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
    
    if(curDim[1]==1&&curDim[2]==1){
      info$dataType=T_scale
      info$size1=1
      info$size2=1
    }else{
      info$dataType=T_matrix
      info$size1=paste0("(nrow(",varInfo$parmsTblName,"[[",i,"]]))")
      info$size2=paste0("(ncol(",varInfo$parmsTblName,"[[",i,"]]))")
    }
    
    
    varInfo=addVarInfo(varInfo,info)
    varInfo$requiredVar=c(varInfo$requiredVar,info$var)
  }
  varInfo
}
renameLoopVar<-function(parsedExp,ind=0){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(!is.call(curExp))
      next
    if(curExp[[1]]=="for"){
      #Force substitution of the index variable
      new_index=paste0(GPUVar$gpu_loop_ind,"_",ind)
      ind=ind+1
      old_Index=deparse(curExp[[2]])
      loopBody=renameVarInCode(curExp[[4]],1,old_Index,new_index)
      res=renameLoopVar(loopBody,ind)
      loopBody=res$parsedExp
      ind=res$ind
      curExp[[2]]=as.symbol(new_index)
      curExp[[4]]=loopBody
      parsedExp[[i]]=curExp
    }
    if(curExp[[1]]=="if"){
      res=renameLoopVar(curExp[[3]],ind)
      codeBody=res$parsedExp
      ind=res$ind
      curExp[[3]]=codeBody
      parsedExp[[i]]=curExp
      if(length(curExp)==4){
        res=renameLoopVar(curExp[[4]],ind)
        codeBody=res$parsedExp
        ind=res$ind
        curExp[[4]]=codeBody
        parsedExp[[i]]=curExp
      }
    }
  }
  return(list(parsedExp=parsedExp,ind=ind))
}

profileLoopVar<-function(varInfo,parsedExp){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(!is.call(curExp))
      next
    if(curExp[[1]]=="for"){
      var_char=deparse(curExp[[2]])
      ExpProfile=getEmpyTable(type = T_scale)
      ExpProfile$var=var_char
      ExpProfile$initialization=FALSE
      ExpProfile$precisionType=gpuMagic.option$getDefaultInt()
      varInfo=addVarInfo(varInfo,ExpProfile)
      loopBody=curExp[[4]]
      varInfo=profileLoopVar(varInfo,loopBody)
    }
    if(curExp[[1]]=="if"){
      varInfo=profileLoopVar(varInfo,curExp[[3]])
      if(length(curExp)==4){
        varInfo=profileLoopVar(varInfo,curExp[[4]])
      }
    }
  }
  return(varInfo)
}

#==================================Profiler 2==========================

#Find the function parameters
#If the functions' argument does not show in the expression, the default value will be used
matchFunArg<-function(fun,Exp){
  funArg=formals(fun)
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
getExpInfo<-function(varInfo,Exp){
  ExpInfo=NULL
  if(is.numeric(Exp)){
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
  if(is.null(ExpInfo))
    stop("Unknow code: ",deparse(Exp))
  
  return(ExpInfo)
}

checkVarType<-function(leftInfo,rightInfo){
  needReassign=FALSE
  needResize=FALSE
  needRetype=FALSE
  if(typeInherit(leftInfo$precisionType,rightInfo$precisionType)!=leftInfo$precisionType)
    needRetype=TRUE
  if(leftInfo$dataType!=rightInfo$dataType)
    return(list(needReassign=TRUE))
  
  if(leftInfo$size1!=rightInfo$size1||rightInfo$size2!=rightInfo$size2){
    len1=paste0(leftInfo$size1,"*",leftInfo$size2)
    len2=paste0(rightInfo$size1,"*",rightInfo$size2)
    if(Simplify(len1)!=Simplify(len2)||
       rightInfo$size1=="NA"||rightInfo$size2=="NA"||
       leftInfo$shared){
      return(list(needReassign=TRUE))
    }else{
        return(list(needRetype=needRetype,needReassign=FALSE,needResize=TRUE,size1=rightInfo$size1,size2=rightInfo$size2))
    }
  }
  return(list(needRetype=needRetype,needReassign=FALSE,needResize=FALSE))
}

#Determine which type can preserve the information 
#of the information in type1 and type2
typeInherit<-function(type1,type2){
  if(!is.character(type1))
    type1=as.character(type1)
  if(!is.character(type2))
    type2=as.character(type2)
  
  group_float=c("half","float","double")
  group_int=c("char","int","long","uint","ulong")
  
  target_size=max(getTypeSize(type1),getTypeSize(type2))
  if(type1 %in% group_float||type2%in% group_float){
    for(i in 1:length(group_float)){
      if(target_size==getTypeSize(group_float[i]))
         return(group_float[i])
    }
  }else{
    if(type1 %in% group_int||type2%in% group_int){
      for(i in 1:length(group_int)){
        if(target_size==getTypeSize(group_int[i]))
          return(group_int[i])
      }
    }else{
      warning("Unsupported type inherit: ",type1,"+",type2)
      return(gpuMagic.option$getDefaultFloat())
    }
  }
}

is.preservedFunc<-function(func){
  func=as.character(func)
  length(grep(GPUVar$preservedFuncPrefix,func,fixed = T))!=0
}
#This function determine when the variable is defined, which property can be inherit from the right expression
copyVarInfo<-function(info){
  info$shared=FALSE
  info$location="global"
  info$version=1
  
  info$require=FALSE
  info$constVal=FALSE
  info$constDef=FALSE
  info$lazyRef=FALSE
  info$initialization=TRUE
  info$changed=FALSE
  info$used=FALSE
  #Some optimization
  if(info$compileSize1&&info$compileSize2&&info$size1=="1"&&info$size2=="1")
    info$dataType=T_scale
  if(info$dataType==T_scale)
    info$location="local"
  
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
#Test is a character is a number
isNumeric<-function(char){
  if(char=="")
    return(FALSE)
  return(!grepl("\\D", char))
}

toCharacter<-function(charOrSym){
  if(!is.character(charOrSym))
    charOrSym=deparse(charOrSym)
  charOrSym
}
Simplify2<-function(Exp){
  res=Simplify(Exp)
  if(isNumeric(res))
    return(res)
  else
    return(paste0("(",res,")"))
}


