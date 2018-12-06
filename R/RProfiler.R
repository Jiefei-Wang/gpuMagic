#' @include pkgFunc.R
#Functions:
#1.rename the loop variable
#2.Profile the function argument and loop variable

RProfile1<-function(codeMetaInfo2){
  profileMeta1=codeMetaInfo2
  
  #The looped variable needs some special treatment, because inside each function it has a unique looped variable
  #The overall looped variable is redefined to distinguish the overall and individual looped variables.
  loopData=names(profileMeta1$parms)[1]
  func_args=profileMeta1$parms
  
  #profile the function arguments and the preserved variable
  varInfo=profileVar(func_args,profileMeta1$macroParms)
  gpuIndex=getEmpyTable(type=T_scale)
  gpuIndex$var=GPUVar$gpu_global_id
  gpuIndex$precisionType=GPUVar$default_index_type
  gpuIndex$initialization=FALSE
  varInfo=addVarInfo(varInfo,gpuIndex)
  profileMeta1$varInfo=varInfo
  
  #Rename the loop var
  parsedExp=profileMeta1$Exp
  res=renameLoopVar(parsedExp)
  parsedExp=res$parsedExp
  #Profile the loop variable
  newVarInfo=profileLoopVar(varInfo,parsedExp)
  profileMeta1$varInfo=newVarInfo
  profileMeta1$Exp=parsedExp
  
  profileMeta1
}



#Function:
#1. Profile the variable type
#2. rename the variable if the type does not match
RProfile2<-function(profileMeta1){
  if(DEBUG){
    profileMeta1$varInfo=copyVarInfoTbl(profileMeta1$varInfo)
  }
  
  profileMeta2=parserFrame(RProfile2_parserFunc,RProfile2_checkFunc,
                           RProfile2_updateFunc,profileMeta1)
  
  profileMeta2
}

RProfile2_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list(Exp=curExp)
  tmpMeta=codeMetaInfo$tmpMeta
  varInfo=codeMetaInfo$varInfo
  renameList=c()
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  result$varInfo=varInfo
  
  
  formattedExp=formatCall(curExp,generalType=FALSE)
  formattedExp_char=gsub(" ", "",deparse(formattedExp), fixed = TRUE)
  #process transpose
  if(formattedExp_char=="var=t(var)"){
    if(curExp[[2]]==curExp[[3]][[2]]){
      curVar=curExp[[2]]
      #Check if the target can be changed
      if(getVarProperty(varInfo,curVar,"constVal")){
        stop("The const value cannot be changed",deparse(curExp))
      }
      if(getVarProperty(varInfo,curVar,"lazyRef")){
        stop("Cannot transpose the laze expression",deparse(curExp))
      }
      
      #set the transpose
      varInfo=versionBump(varInfo,curVar)
      curTranspose=getVarProperty(varInfo,curVar,"transpose")
      curTranspose=!curTranspose
      varInfo=setVarProperty(varInfo,curVar,"transpose",curTranspose)
      
      
      #set the version bump
      var_char=deparse(curVar)
      version=getVarProperty(varInfo,curVar,"version")
      versionBump=parse(text=paste0(GPUVar$preservedFuncPrefix,"setVersion(",var_char,",",version,")"))[[1]]
      
      result$varInfo=varInfo
      result$extCode=versionBump
      return(result)
    }
  }
  #stop when the code is like A=B%*%A, 
  #it is unsafe to do the operation and assign back the result to the original matrix
  if(formattedExp_char=="var=var%*%var"){
    if(curExp[[2]]==curExp[[3]][[3]]){
      stop("The the left variable cannot be the same as the right variable:\n",deparse(curExp))
    }
  }
  
  
  
  if(curExp[[1]]=="="){ 
    leftExp=curExp[[2]]
    if(is.symbol(leftExp)){
      #profile the left symbol
      res=profile_symbol_left(level,codeMetaInfo,varInfo,curExp)
      for(i in names(res)){
        result[[i]]=res[[i]]
      }
      return(result)
    }
    
    
  }
  
  if(curExp[[1]]=="return"){
    returnInfo=getVarInfo(varInfo,curExp[[2]])
    varInfo$returnInfo=returnInfo
  }
  
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  result$renameList=renameList
  result$varInfo=varInfo
  return(result)
}


#Exp="b*a[1,10]+c(4,43)[1]+10-1"
#Simplify(Exp)

#Filter the preserved functions
RProfile2_checkFunc<-function(curExp){
  if(!is.call(curExp)){
    return(FALSE)
  }
  if(is.preservedFunc(curExp))
    return(FALSE)
  return(TRUE)
}

RProfile2_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result$codeMetaInfo$varInfo=res$varInfo
  result
}
