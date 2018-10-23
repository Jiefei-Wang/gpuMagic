#codeMetaInfo=curMetaInfo
if(FALSE){
parserFunc=RPP_parserFunc
checkFunc=RPP_checkFunc
updateFunc=RPP_updateFunc
codeMetaInfo=profileMeta1
level=c("top")
}
parserFrame<-function(parserFunc,checkFunc,updateFunc,codeMetaInfo,level=c("top")){
  curCodeMetaInfo=codeMetaInfo
  parsedExp=codeMetaInfo$Exp
  code=c()
  for(i in seq(length(parsedExp))){
    #if(i==3) stop()
    curExp=parsedExp[[i]]
    if(curExp=="{"){
      next
      }
    if(!is.symbol(curExp)){
      if(curExp[[1]]=="for"){
        loopIndExp=curExp[[3]]
        curLevel=c(level,"for")
        if(checkFunc(loopIndExp)){
          res=ProcessCodeSingle(parserFunc,updateFunc,
                                curCodeMetaInfo,curLevel,parsedExp,code,i,loopIndExp)
          curCodeMetaInfo=res$codeMetaInfo
          parsedExp=res$parsedExp
          code=res$code
          loopIndExp=res$Exp
        }
        loopBodyExp=curExp[[4]]
        res=ProcessCodeChunk(parserFunc,checkFunc,updateFunc,
                             curCodeMetaInfo,curLevel,parsedExp,code,i,loopBodyExp)
        curCodeMetaInfo=res$codeMetaInfo
        parsedExp=res$parsedExp
        code=res$code
        loopBodyExp=res$ExpChunk
        
        
        curExp[[3]]=loopIndExp
        curExp[[4]]=loopBodyExp
        code=c(code,curExp)
        next
      }
      
      
      if(curExp[[1]]=="if"){
        conditionExp=curExp[[2]]
        curLevel=c(level,"if")
        if(checkFunc(loopInd)){
          res=ProcessCodeSingle(parserFunc,updateFunc,
                                curCodeMetaInfo,curLevel,parsedExp,code,i,conditionExp)
          curCodeMetaInfo=res$codeMetaInfo
          parsedExp=res$parsedExp
          code=res$code
          conditionExp=res$Exp
        }
        for(k in 3:4){
          if(k>length(curExp))
            next
          conditionBodyExp=curExp[[k]]
          res=ProcessCodeChunk(parserFunc,checkFunc,updateFunc,
                               curCodeMetaInfo,curLevel,parsedExp,code,i,conditionBodyExp)
          curCodeMetaInfo=res$codeMetaInfo
          parsedExp=res$parsedExp
          code=res$code
          conditionBodyExp=res$ExpChunk
          
          curExp[[k]]=conditionBodyExp
        }
        
        curExp[[2]]=conditionExp
        code=c(code,curExp)
        next
      }
    }
    if(checkFunc(curExp)){
      res=ProcessCodeSingle(parserFunc,updateFunc,
                            curCodeMetaInfo,level,parsedExp,code,i,curExp)
      curCodeMetaInfo=res$codeMetaInfo
      parsedExp=res$parsedExp
      code=c(res$code,res$Exp)
      next
    }
  }
  curCodeMetaInfo$Exp=code
  return(curCodeMetaInfo)
}

ProcessCodeChunk<-function(parserFunc,checkFunc,updateFunc,codeMetaInfo,curLevel,parsedExp,code,i,ExpChunk){
  curMetaInfo=codeMetaInfo
  curMetaInfo$Exp=ExpChunk
  curMetaInfo$renameList=NULL
  res=parserFrame(parserFunc,checkFunc,updateFunc,curMetaInfo,curLevel)
  if("renameList" %in% names(res)){
    parsedExp=renamevariable(parsedExp,res$renameList,i)
    codeMetaInfo$renameList=rbind(codeMetaInfo$renameList,res$renameList)
    }
  res1=updateFunc(type="block",curLevel,codeMetaInfo,parsedExp,code,i,res)
  if("codeMetaInfo" %in% names(res1))
    codeMetaInfo=res1$codeMetaInfo
  if("parsedExp" %in% names(res1))
    parsedExp=res1$parsedExp
  if("code" %in% names(res1))
    code=res1$code
  ExpChunk=compressCodeChunk(res$Exp)
  
  list(codeMetaInfo=codeMetaInfo,parsedExp=parsedExp,code=code,ExpChunk=ExpChunk)
}
ProcessCodeSingle<-function(parserFunc,updateFunc,codeMetaInfo,curLevel,parsedExp,code,i,Exp){
  res=parserFunc(curLevel,codeMetaInfo,Exp)
  if("renameList" %in% names(res)){
    parsedExp=renamevariable(parsedExp,res$renameList,i)
    codeMetaInfo$renameList=rbind(codeMetaInfo$renameList,res$renameList)
  }
  Exp=res$Exp
  code=c(code,res$extCode)
  res1=updateFunc(type="normal",curLevel,codeMetaInfo,parsedExp,code,i,res)
  codeMetaInfo=res1$codeMetaInfo
  parsedExp=res1$parsedExp
  if("codeMetaInfo" %in% names(res1))
    codeMetaInfo=res1$codeMetaInfo
  if("parsedExp" %in% names(res1))
    parsedExp=res1$parsedExp
  if("code" %in% names(res1))
    code=res1$code
  
  list(codeMetaInfo=codeMetaInfo,parsedExp=parsedExp,code=code,Exp=Exp)
}
renamevariable<-function(parsedExp,renameList,i){

 for(i in 1:nrow(renameList))
  parsedExp=renameVarInCode(parsedExp,i,renameList[i,1],renameList[i,2])
  parsedExp
}


general_updateFunc<-function(codeMetaInfo,parsedExp,code){
  result=list()
  result$codeMetaInfo=codeMetaInfo
  result$parsedExp=parsedExp
  result$code=code
  result
}