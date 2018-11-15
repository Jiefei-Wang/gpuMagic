codePreprocessing<-function(codeMetaInfo){
  loopVar=names(codeMetaInfo$parms)[[1]]
  names(codeMetaInfo$parms)[[1]]=GPUVar$gpu_loop_data
  #insert the preserved data reading code
  readDataExp=parse(text=paste0(loopVar,"=",GPUVar$gpu_loop_data,"[",GPUVar$gpu_global_id,"+1]"))
  codeMetaInfo$Exp=c(readDataExp,codeMetaInfo$Exp)
  
  
  codeMetaInfo
}
#Level 1 compiler
#Functions:
#1.simplify the R code, each line should only have one function call,
#If not, a temporary variable will be created to replace it.
#2.If the code only has a symbol and the symbol is not recognized, it will be removed
RParser1<-function(codeMetaInfo,tmpMeta=NULL){
  if(is.null(tmpMeta)){
    codeMetaInfo$tmpMeta=list(count=1)
  }else{
    codeMetaInfo$tmpMeta=tmpMeta
  }
  codeMetaInfo1=parserFrame(RLevel1_parserFunc,RLevel1_checkFunc,
                            RLevel1_updateFunc,codeMetaInfo)
  codeMetaInfo1
}


RLevel1_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  tmpMeta=codeMetaInfo$tmpMeta
  
  
  code_char=deparse(curExp)[1]
  if(substr(code_char,1,7)==GPUVar$openclCode){
    result$Exp=curExp
    result$tmpMeta=tmpMeta
    return(result)
  }
  
  if(!is.symbol(curExp)){
    if(curExp[[1]]=="="||curExp[[1]]=="=="){
      for(j in 2:3){
        oneSideExp=curExp[[j]]
        if(length(oneSideExp)>=2){
          for(i in seq(2,length(oneSideExp))){
            if(is.call(oneSideExp[[i]])){
              res=createNewVar(tmpMeta,oneSideExp[[i]])
              tmpMeta=res$tmpMeta
              result$extCode=c(result$extCode,res$code)
              curExp[[j]][[i]]=as.symbol(tmpMeta$varName)
            }
          }
        }
      }
      result$Exp=curExp
      result$tmpMeta=tmpMeta
      return(result)
    }
    #General strategy for all functions that do not appear above
    if(length(curExp)>1)
      for(i in 2:length(curExp)){
        if(is.call(curExp[[i]])){
          res=createNewVar(tmpMeta,curExp[[i]])
          tmpMeta=res$tmpMeta
          result$extCode=c(result$extCode,res$code)
          curExp[[i]]=as.symbol(tmpMeta$varName)
        }
      }
  }else{
    if(curExp!="break"||curExp!="next")
      curExp=NULL
  }
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  return(result)
}

RLevel1_checkFunc<-function(curExp){
  return(TRUE)
}

RLevel1_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result
}




#Level2 compiler
#Functions:
#1.For "=" sign: If the left side symbol is subsetted, 
#the right side symbol should be a symbol,
#if not, create a temporary function to replace it
RParser2<-function(codeMetaInfo1){
  codeMetaInfo2=parserFrame(RLevel2_parserFunc,RLevel2_checkFunc,
                            RLevel2_updateFunc,codeMetaInfo1)
  codeMetaInfo2
}



RLevel2_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  tmpMeta=codeMetaInfo$tmpMeta
  
  if(!is.symbol(curExp)){
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      if(is.call(leftExp)&&is.call(rightExp))
      {
        res=createNewVar(tmpMeta,rightExp)
        tmpMeta=res$tmpMeta
        result$extCode=c(result$extCode,res$code)
        curExp[[3]]=as.symbol(tmpMeta$varName)
      }
      result$Exp=curExp
      result$tmpMeta=tmpMeta
      return(result)
    }
    
    
    if(curExp[[1]]=="=="){
      for(i in 2:3){
        oneSideExp=curExp[[i]]
        if(is.call(oneSideExp))
        {
          res=createNewVar(tmpMeta,oneSideExp)
          tmpMeta=res$tmpMeta
          result$extCode=c(result$extCode,res$code)
          curExp[[i]]=as.symbol(tmpMeta$varName)
        }
      }
      result$Exp=curExp
      result$tmpMeta=tmpMeta
      return(result)
    }
  }
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  return(result)
}

RLevel2_checkFunc<-function(curExp){
  return(TRUE)
}

RLevel2_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result
}













