#Level 1 compiler
#Functions:
#1.simplify the R code, each line should only have one function call,
#If not, a temporary variable will be created to replace it.
#2.If the code only has a symbol, it will be remove from the function
RRParser1_new<-function(codeMetaInfo,insertPreserved=T){
  #define the looped variable
  loopVar=names(codeMetaInfo$parms)[[1]]
  #insert the preserved data reading code
  if(insertPreserved){
    readDataExp=parse(text=paste0(loopVar,"=",GPUVar$gpu_worker_data,"[",GPUVar$gpu_global_id,"+1]"))
    codeMetaInfo$Exp=c(readDataExp,codeMetaInfo$Exp)
  }
  
  codeMetaInfo$tmpMeta=list(count=1)
  codeMetaInfo1=parserFrame(RRLevel1_parserFunc,RRLevel1_checkFunc,
                            RRLevel1_updateFunc,codeMetaInfo)
  codeMetaInfo1
}


RRLevel1_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  tmpMeta=codeMetaInfo$tmpMeta
  
  
  if(curExp[[1]]=="="||curExp[[1]]=="=="){
    for(j in 2:3){
      oneSideExp=curExp[[j]]
      if(length(oneSideExp)>=2){
        for(i in seq(2,length(oneSideExp))){
          if(is.call(oneSideExp[[i]])){
            res=createNewVarLevel1_test(tmpMeta,oneSideExp[[i]])
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
  for(i in 2:length(curExp)){
    if(is.call(curExp[[i]])){
      res=createNewVarLevel1_test(tmpMeta,curExp[[i]])
      tmpMeta=res$tmpMeta
      result$extCode=c(result$extCode,res$code)
      curExp[[i]]=as.symbol(tmpMeta$varName)
    }
  }
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  return(result)
}

RRLevel1_checkFunc<-function(curExp){
  return(TRUE)
}

RRLevel1_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result
}




#Level2 compiler
#Functions:
#1.For "=" sign: If the left side symbol is subsetted, 
#the right side symbol should be a symbol,
#if not, create a temporary function to replace it
RRParser2_new<-function(codeMetaInfo1){
  codeMetaInfo2=parserFrame(RRLevel2_parserFunc,RRLevel2_checkFunc,
                            RRLevel2_updateFunc,codeMetaInfo1)
  codeMetaInfo2
}



RRLevel2_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  tmpMeta=codeMetaInfo$tmpMeta
  
  if(curExp[[1]]=="="){
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    if(is.call(leftExp)&&is.call(rightExp))
    {
      res=createNewVarLevel1_test(tmpMeta,rightExp)
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
        res=createNewVarLevel1_test(tmpMeta,oneSideExp)
        tmpMeta=res$tmpMeta
        result$extCode=c(result$extCode,res$code)
        curExp[[i]]=as.symbol(tmpMeta$varName)
      }
    }
    result$Exp=curExp
    result$tmpMeta=tmpMeta
    return(result)
  }
  
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  return(result)
}

RRLevel2_checkFunc<-function(curExp){
  return(TRUE)
}

RRLevel2_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  result
}



#Level3 compiler
#Functions:
#1.Rename the variable if the variable is redefined
#2.Rename the loop index
#
RRParser3_new<-function(codeMetaInfo2){
  
  parms=codeMetaInfo2$parms
  parmsList=as.list(names(parms))
  names(parmsList)=names(parms)
  #Remove the looped variable
  parmsList=parmsList[-1]
  
  #register the preserved variable
  parmsList[[GPUVar$gpu_global_id]]=GPUVar$gpu_global_id
  parmsList[[GPUVar$gpu_worker_data]]=GPUVar$gpu_worker_data
  
  codeMetaInfo2$varList=hash(parmsList)
  
  codeMetaInfo3=parserFrame(RRLevel3_parserFunc,RRLevel3_checkFunc,
              RRLevel3_updateFunc,codeMetaInfo2)
  
  #Rename the loop var
  parsedExp=codeMetaInfo2$Exp
  res=renameLoopVar(parsedExp)
  
  codeMetaInfo3$Exp=res$parsedExp
  codeMetaInfo3$parms=parms
  
  
  codeMetaInfo3
}

RRLevel3_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  tmpMeta=codeMetaInfo$tmpMeta
  renameList=c()
  varList=codeMetaInfo$varList
  
  if(level[length(level)]=="for"){
    result$Exp=curExp
    result$tmpMeta=tmpMeta
    result$renameList=renameList
    result$varList=varList
    return(result)
  }
  if(curExp[[1]]=="="){ 
    leftExp=curExp[[2]]
    rightExp=curExp[[3]]
    var_char=deparse(leftExp)
    if(!is.call(leftExp)){
      if(has.key(var_char,varList)){
        tmpMeta=getTmpName_test(tmpMeta)
        tmpName=tmpMeta$varName
        #parsedExp=renameVarInCode(parsedExp,i+1,var_char,tmpName)
        renameList=rbind(renameList,c(length(level)+1,var_char,tmpName))
        curExp[[2]]=as.symbol(tmpName)
        varList[[tmpName]]=tmpName
      }else{
        varList[[var_char]]=var_char
      }
    }
  }
  result$Exp=curExp
  result$tmpMeta=tmpMeta
  result$renameList=renameList
  result$varList=varList
  return(result)
}
  

RRLevel3_checkFunc<-function(curExp){
  #if(curExp[[1]]=="=")return(TRUE)
  return(TRUE)
}

RRLevel3_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result$codeMetaInfo$tmpMeta=res$tmpMeta
  varList=result$codeMetaInfo$varList
  for(i in keys(res$varList)){
    varList[[i]]=i
  }
  result$codeMetaInfo$varList=varList
  
  renameList=res$renameList
  
  
  curLevel=length(level)
  ind=c()
  if(!is.null(renameList))
  for(i in nrow(renameList):1){
    renameLevel=renameList[i,1]
    if(curLevel<renameLevel){
      old_name=renameList[i,2]
      new_name=renameList[i,3]
      parsedExp=renameVarInCode(parsedExp,i+1,old_name,new_name)
      renameList[i,1]=renameLevel
      ind=c(ind,i)
    }else{
      break
    }
  }
  
  result$codeMetaInfo$renameList=rbind(result$codeMetaInfo$renameList,renameList[ind,])
  
  result$parsedExp=parsedExp
  result
}












