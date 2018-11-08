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
  varInfo=profileVar(func_args,profileMeta1$constantParms)
  gpuIndex=getEmpyTable(1,type=T_scale)
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
    profileMeta1$varInfo$varTable=copy(profileMeta1$varInfo$varTable)
    profileMeta1$varInfo$varVersion=copy(profileMeta1$varInfo$varVersion)
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
  
  
  if(curExp[[1]]=="="){ 
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    leftVar_char=deparse(leftExp)
    if(!is.call(leftExp)){
      if(has.key(leftVar_char,varInfo$varVersion)){
        leftInfo=getVarInfo(varInfo,leftVar_char)
        rightInfo=getExpInfo(varInfo,rightExp)
        checkInfo=checkVarType(leftInfo,rightInfo)
        if(checkInfo$needReassign){
          if("for" %in% level || "if" %in% level)
            stop("Type conversion inside the for or if body is not allowed, please assign the variable before it:\n:",
                 paste0("TraceBack:",paste0(level,collapse = "->"),"\n"),
                 deparse(curExp))
          if(leftInfo$constant=="Y")
            stop("The left expression is a constant, changing the number of the left expression is not allowed:\n:",deparse(curExp))
          tmpMeta=getTmpVar(tmpMeta)
          newName=tmpMeta$varName
          curExp[[2]]=as.symbol(newName)
          renameList=rbind(renameList,c(leftVar_char,newName))
          leftInfo=copyVarInfo(rightInfo)
          leftInfo$var=newName
          varInfo=addVarInfo(varInfo,leftInfo)
        }else{
          if(checkInfo$needResize){
            versionBump=paste0(GPUVar$preservedFuncPrefix,"bump(",leftVar_char,")")
            resizeCode=paste0(GPUVar$preservedFuncPrefix,"resize(",leftVar_char,",",checkInfo$size1,",",checkInfo$size2,")")
            result$extCode=c(result$extCode,versionBump,resizeCode)
          }
          if(checkInfo$needRetype){
            leftInfo$precisionType=rightInfo$precisionType
          }
          leftInfo$value=rightInfo$value
          leftInfo$compileData=rightInfo$compileData
          if(checkInfo$needResize)
            varInfo=addVarInfo(varInfo,leftInfo)
          else
            varInfo=setVarInfo(varInfo,leftInfo)
        }
      }else{
        rightInfo=getExpInfo(varInfo,rightExp)
        leftInfo=copyVarInfo(rightInfo)
        leftInfo$var=leftVar_char
        leftInfo$version=1
        varInfo=addVarInfo(varInfo,leftInfo)
      }
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
  if(is.call(curExp)&&is.preservedFunc(curExp[[1]])){
    return(FALSE)
  }
  return(TRUE)
}

RProfile2_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result$codeMetaInfo$varInfo=res$varInfo
  result
}
