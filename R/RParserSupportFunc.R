#==============================parser 1==========================

#parsedExp=parse(text="f(g(),a)")[[1]]
#Create a new variable to represent a function call
createNewVarLevel1_test<-function(tmpMeta,parsedExp){
  tmpMeta=getTmpName_test(tmpMeta)
  curName=tmpMeta$varName
  curCode=c()
  if(length(parsedExp)>1){
    for(i in seq(2,length(parsedExp))){
      #If the argument is also a function call
      curArg=parsedExp[[i]]
      if(is.call(curArg)){
        res=createNewVarLevel1_test(tmpMeta,curArg)
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


getTmpName_test<-function(tmpMeta){
  count=tmpMeta$count
  return(list(count=count+1,varName=paste0("opencl_tmp_",count)))
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
renameLoopVar<-function(parsedExp,ind=0){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(is.call(curExp)&&curExp[[1]]=="for"){
      #Force substitution of the index variable
      new_index=paste0(GPUVar$gpu_loop_ind,"_",ind)
      ind=ind+1
      old_Index=deparse(curExp[[2]])
      loopBody=renameVarInCode(curExp[[4]],1,old_Index,new_index)
      res=renameLoopVar(loopBody,ind)
      loopBody=res$parsedExp
      ind=res$ind
      curExp[[2]]=as.symbol(new_index)
      curExp[[4]]=loopBody
      parsedExp[[i]]=curExp
    }
  }
  return(list(parsedExp=parsedExp,ind=ind))
}


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