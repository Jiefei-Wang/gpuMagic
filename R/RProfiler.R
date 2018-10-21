

#Profile the function arguments only
RProfiler1<-function(codeMetaInfo3){
  profileMeta1=codeMetaInfo3
  
  #The looped variable needs some special treatment, I rename it so that it would not be confused with the individual looped variable
  loopData=names(codeMetaInfo3$parms)[1]
  func_args=c(profileMeta1$parms)
  names(func_args)[1]=GPUVar$gpu_worker_data
  
  
  varInfo=profileVar(func_args)
  gpuIndex=getEmpyTable(1,type=T_scale)
  gpuIndex$var=GPUVar$gpu_global_id
  gpuIndex$precisionType=T_I64
  gpuIndex$initialization="N"
  varInfo$profile=rbind(varInfo$profile,gpuIndex)
  varInfo$varTable[[gpuIndex$var]]=nrow(varInfo$profile)
  
  profileMeta1$varInfo=varInfo
  ###############This need to be removed later################################
  #profileMeta1$workerData=loopData
  
  return(profileMeta1)
}





RProfiler2<-function(profileMeta1){
  #Profile the loop variable
  varInfo=profileMeta1$varInfo
  parsedExp=profileMeta1$Exp
  res=profileLoopVar(varInfo,parsedExp)
  profileMeta1$varInfo=res
  
  profileMeta2=parserFrame(RProfiler2_parserFunc,RProfiler2_checkFunc,
                           RProfiler2_updateFunc,profileMeta1)
  profileMeta2
}

RProfiler2_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  result$Exp=curExp
  varInfo=codeMetaInfo$varInfo
  
  if(curExp[[1]]=="="){
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    #Extract the variable in the left expression
    if(is.call(leftExp)){
      if(leftExp[[1]]!="[")
        stop("Unrecognized code",deparse(curExp))
    }else{
      var_char=deparse(leftExp)
      #Check if the variable is in the variable table, if not, profile the variable
      if(!has.key(var_char,varInfo$varTable)){
        ExpProfile=getExpInfo(varInfo,rightExp)
        ExpProfile$var=var_char
        varInfo$profile=rbind(varInfo$profile,ExpProfile)
        varInfo$varTable[[var_char]]=nrow(varInfo$profile)
      }
    }
    result$varInfo=varInfo
    return(result)
  }
  
  if(curExp[[1]]=="return"){
    if(length(level)!=1)
      stop("Unsupported code, the return command is only allowed in the end of the code:\n",deparse(curExp))
    ExpProfile=profile_return(varInfo,curExp)
    varInfo$returnInfo=ExpProfile
    result$varInfo=varInfo
    return(result)
  }
  
  result$varInfo=varInfo
  return(result)
}


RProfiler2_checkFunc<-function(curExp){
  #if(curExp[[1]]=="=")return(TRUE)
  return(TRUE)
}

RProfiler2_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$varInfo=res$varInfo
  result
}







