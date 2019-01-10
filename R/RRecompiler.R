RRecompiler<-function(profileMeta2){
  profileMeta3=profileMeta2
  if(DEBUG)
    profileMeta3$varInfo=copyVarInfoTbl(profileMeta3$varInfo)
  
  profileMeta3$varInfo$curVarVersion=copy(profileMeta3$varInfo$varVersion)
  for(i in keys(profileMeta3$varInfo$curVarVersion)){
    profileMeta3$varInfo$curVarVersion[[i]]=1
  }
  profileMeta3=parserFrame(RRE_parserFunc,RRE_checkFunc,
                          RRE_updateFunc,profileMeta3)
  
  
  codeMetaInfo=list()
  codeMetaInfo$Exp=profileMeta3$Exp
  codeMetaInfo$parms=profileMeta3$parms
  codeMetaInfo$staticParms=profileMeta3$staticParms
  codeMetaInfo1=RParser1(codeMetaInfo)
  #codeMetaInfo2=RParser2(codeMetaInfo1)
  profileMeta1=RProfile1(codeMetaInfo2)
  profileMeta2=RProfile2(profileMeta1)
  
  
  
  
  profileMeta2
}

RRE_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list(Exp=curExp)
  varInfo=codeMetaInfo$varInfo
  FuncName=deparse(curExp[[1]])
  if(FuncName==paste0(GPUVar$preservedFuncPrefix,"setVersion")){
    codeMetaInfo$varInfo$curVarVersion[[deparse(curExp[[2]])]]=as.numeric(deparse(curExp[[3]]))
    return(result)
  }
  
  
  if(FuncName=="="){
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    if(is.call(leftExp)){
      return(result)
    }
    #Extract the variable in the left expression
    if(is.call(rightExp)){
      rightFunc=deparse(rightExp[[1]])
      if(!is.null(.recompileFuncs[[rightFunc]])){
        curCode=.recompileFuncs[[rightFunc]](varInfo,curExp)
        if(length(curCode)>1){
          for(i in 1:(length(curCode)-1))
            result$extCode=c(result$extCode,curCode[[i]])
        }
        curExp=curCode[[length(curCode)]]
      }
    }
  }
  
  
  if(!is.null(.recompileFuncs[[FuncName]])){
    curCode=.recompileFuncs[[rightFunc]](varInfo,curExp)
    if(length(curCode)>1){
      for(i in 1:(length(curCode)-1))
        result$extCode=c(result$extCode,curCode[[i]])
    }
    curExp=curCode[[length(curCode)]]
  }
  
  result$Exp=curExp
  return(result)
}


RRE_checkFunc<-function(curExp){
  #if(curExp[[1]]=="=")return(TRUE)
  if(!is.call(curExp)) return(FALSE)
  return(TRUE)
}

RRE_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result
}
