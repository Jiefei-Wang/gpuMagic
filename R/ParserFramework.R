#codeMetaInfo=curMetaInfo
if(FALSE){
parserFunc=RProfiler2_parserFunc
checkFunc=RProfiler2_checkFunc
updateFunc=RProfiler2_updateFunc
codeMetaInfo=profileMeta1
level=c("top")
}
parserFrame<-function(parserFunc,checkFunc,updateFunc,codeMetaInfo,level=c("top")){
  curCodeMetaInfo=codeMetaInfo
  parsedExp=codeMetaInfo$Exp
  code=c()
  for(i in seq(length(parsedExp))){
    curExp=parsedExp[[i]]
    #if(i==3) stop()
    if(curExp=="{"){
      next
      }
    if(!is.symbol(curExp)){
      if(curExp[[1]]=="for"){
        loopIndExp=curExp[[3]]
        curLevel=c(level,"for")
        if(checkFunc(loopIndExp)){
          res=parserFunc(curLevel,curCodeMetaInfo,loopIndExp)
          loopIndExp=res$Exp
          code=c(code,res$extCode)
          res1=updateFunc(type="normal",curLevel,curCodeMetaInfo,parsedExp,code,i,res)
          curCodeMetaInfo=res1$codeMetaInfo
          parsedExp=res1$parsedExp
          if("code" %in% names(res1))
            code=res1$code
        }
        loopBodyExp=curExp[[4]]
        curLevel=c(level,"for")
        curMetaInfo=curCodeMetaInfo
        curMetaInfo$Exp=loopBodyExp
        res=parserFrame(parserFunc,checkFunc,updateFunc,curMetaInfo,curLevel)
        res1=updateFunc(type="block",curLevel,curCodeMetaInfo,parsedExp,code,i,res)
        curCodeMetaInfo=res1$codeMetaInfo
        parsedExp=res1$parsedExp
        if("code" %in% names(res1))
          code=res1$code
        
        
        loopBodyExp=compressCodeChunk(res$Exp)
        curExp[[3]]=loopIndExp
        curExp[[4]]=loopBodyExp
        code=c(code,curExp)
        next
      }
      
      
      if(curExp[[1]]=="if"){
        conditionExp=curExp[[2]]
        curLevel=c(level,"if")
        if(checkFunc(loopInd)){
          res=parserFunc(curLevel,curCodeMetaInfo,conditionExp)
          conditionExp=res$Exp
          code=c(code,res$extCode)
          res1=updateFunc(type="normal",curLevel,curCodeMetaInfo,parsedExp,code,i,res)
          curCodeMetaInfo=res1$codeMetaInfo
          parsedExp=res1$parsedExp
          if("code" %in% names(res1))
            code=res1$code
        }
        loopBodyExp=curExp[[3]]
        curMetaInfo=curCodeMetaInfo
        curMetaInfo$Exp=loopBodyExp
        res=parserFrame(parserFunc,checkFunc,updateFunc,curMetaInfo,curLevel)
        res1=updateFunc(type="block",curLevel,curCodeMetaInfo,parsedExp,code,i,res)
        curCodeMetaInfo=res1$codeMetaInfo
        parsedExp=res1$parsedExp
        if("code" %in% names(res1))
          code=res1$code
        
        loopBodyExp=compressCodeChunk(res$Exp)
        curExp[[2]]=conditionExp
        curExp[[3]]=loopBodyExp
        code=c(code,curExp)
        next
      }
    }
    if(checkFunc(curExp)){
      res=parserFunc(level,curCodeMetaInfo,curExp)
      code=c(code,res$extCode,res$Exp)
      res1=updateFunc(type="normal",level,curCodeMetaInfo,parsedExp,code,i,res)
      curCodeMetaInfo=res1$codeMetaInfo
      parsedExp=res1$parsedExp
      if("code" %in% names(res1))
        code=res1$code
      next
    }
  }
  curCodeMetaInfo$Exp=code
  return(curCodeMetaInfo)
}


general_updateFunc<-function(codeMetaInfo,parsedExp,code){
  result=list()
  result$codeMetaInfo=codeMetaInfo
  result$parsedExp=parsedExp
  result$code=code
  result
}