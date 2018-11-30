#==============================parser 1==========================

#parsedExp=parse(text="f(g(),a)")[[1]]
#Create a new variable to represent a function call
createNewVar<-function(tmpMeta,parsedExp){
  tmpMeta=getTmpVar(tmpMeta)
  curName=tmpMeta$varName
  curCode=c()
  if(length(parsedExp)>1){
    for(i in seq(2,length(parsedExp))){
      #If the argument is also a function call
      curArg=parsedExp[[i]]
      curArg_char=deparse(parsedExp[[i]])
      if(curArg_char!=""&&is.call(curArg)){
        res=createNewVar(tmpMeta,curArg)
        tmpMeta=res$tmpMeta
        #change the argument to a parameter
        parsedExp[[i]]=as.symbol(tmpMeta$varName)
        curCode=c(curCode,res$code)
      }
    }
  }
  curCode=c(
    curCode,
    parse(text=paste0(curName,"=",deparse(parsedExp,backtick=F)))
  )
  tmpMeta$varName=curName
  return(list(tmpMeta=tmpMeta,code=curCode))
}


getTmpVar<-function(tmpMeta){
  count=tmpMeta$count
  return(list(count=count+1,varName=paste0("gpu_tmp_",count)))
}

compressCodeChunk<-function(Exp){
  code=c()
  for(i in 1:length(Exp))
    code=c(code,deparse(Exp[[i]]))
  code=c("{",code,"}")
  code=paste0(code,collapse = "\n")
  return(parse(text=code)[[1]])
}
#convert a function to an expression
funcToExp<-function(f){
  charExp=deparse(f)
  parsedExp=parse(text=charExp)[[1]]
  args=parsedExp[[2]]
  code=as.list(parsedExp[[3]])
  if(code[[1]]=="{") code=code[-1]
  return(list(args=args,code=code))
}

printExp<-function(Exp){
  for(i in Exp){
    message(deparse(i))
  }
}

#================parser 3=================


renameVarInCode<-function(code,start,oldName,newName){
  oldName=as.character(oldName)
  newName=as.symbol(newName)
  if(start<=length(code)){
    for(i in start:length(code)){
      renameList=list(newName)
      names(renameList)=oldName
      code[[i]]=do.call('substitute', list(code[[i]], renameList))
    }
  }
  return(code)
}