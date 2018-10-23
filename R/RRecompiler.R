RRecompiler<-function(profileMeta2){
  profileMeta=profileMeta2
  if(DEBUG)
    profileMeta2$varInfo$varTable=copy(profileMeta2$varInfo$varTable)
  
  profileMeta=parserFrame(RRE_parserFunc,RRE_checkFunc,
                          RRE_updateFunc,profileMeta)
  
  
  
  codeMetaInfo=list()
  codeMetaInfo$Exp=profileMeta$Exp
  codeMetaInfo$parms=profileMeta$parms
  codeMetaInfo$staticParms=profileMeta$staticParms
  codeMetaInfo1=RParser1(codeMetaInfo)
  codeMetaInfo2=RParser2(codeMetaInfo1)
  profileMeta1=RProfile1(codeMetaInfo2)
  profileMeta2=RProfile2(profileMeta1)
  
  
  
  
  profileMeta2
}

RRE_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list(Exp=curExp)
  
  FuncName=deparse(curExp[[1]])
  
  if(FuncName=="="){
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    #Extract the variable in the left expression
    if(is.call(leftExp)){
      if(leftExp[[1]]!="[")
        stop("Unrecognized code",deparse(curExp))
    }else{
      if(is.call(rightExp)){
        rightFunc=deparse(rightExp[[1]])
        if(!is.null(.recompileFuncs[[rightFunc]])){
          curCode=.recompileFuncs[[rightFunc]](varInfo,curExp)
          if(length(curCode)>1){
            for(i in 1:(length(curCode)-1))
              result$extCode=c(result$extCode,curCode[[i]])
          }
          curExp=curCode[[length(curCode)]]
        }else{
          ##########################Need to be solved in future############################
          if(!DEBUG){
            stop("Unsupported code: ",deparse(curExp))
          }
        }
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
  return(TRUE)
}

RRE_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result
}
