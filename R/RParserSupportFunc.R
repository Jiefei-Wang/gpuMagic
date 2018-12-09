#==============================preprocess==========================
renameControlCode<-function(parsedExp){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(is.call(curExp)){
      if(curExp=="break")
        parsedExp[[i]]=quote(opencl_break)
      if(curExp=="next")
        parsedExp[[i]]=quote(opencl_next)
    }else{
      next
    }
    if(curExp[[1]]=="for"){
      curExp[[4]]=renameControlCode(curExp[[4]])
      parsedExp[[i]]=curExp
    }
    if(curExp[[1]]=="if"){
      curExp[[3]]=renameControlCode(curExp[[3]])
      if(length(curExp)==4){
        curExp[[4]]=renameControlCode(curExp[[4]])
      }
      parsedExp[[i]]=curExp
    }
  }
  return(parsedExp)
}

#==============================parser 1==========================

#parsedExp=parse(text="(tmp + tmp1) * tmp")[[1]]
#Create a new variable to represent a function call
createNewVar<-function(tmpMeta,parsedExp){
  parsedExp=cleanExp(parsedExp)
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
        parsedExp[[i]]=as.symbol(res$targetName)
        curCode=c(curCode,res$code)
      }
    }
  }
  
  replaceCode=parse(text=paste0(curName,"=",deparse(parsedExp)))[[1]]
  if(parsedExp[[1]]=="["){
    subsetArgs=matchBracketFunc(parsedExp)
    if(is.null(subsetArgs$j)){
      replaceCode=parse(text=paste0(curName,"=subRef(",parsedExp[[2]],",",subsetArgs$i,")"))[[1]]
    }else{
      replaceCode=parse(text=paste0(curName,"=subRef(",parsedExp[[2]],",",subsetArgs$i,",",subsetArgs$j,")"))[[1]]
    }
  }
  
  
  curCode=c(
    curCode,
    replaceCode
  )
  #tmpMeta$varName=curName
  return(list(targetName=curName,tmpMeta=tmpMeta,code=curCode))
}

#Exp=quote(a[1,])
#This function will return a list of the arguments of the [] function, all the argments are expressed in character
#example: 
#a[1]     ==>i="1",drop="TRUE"
#a[1,]    ==>i="1",b="",drop="TRUE"
matchBracketFunc<-function(Exp){
  res=list(drop="TRUE")
  argName=names(Exp)
  argList=c("i","j","drop")
  
  if(is.null(argName)){
    if(length(Exp)<3)
      stop("Unexpected expression:", deparse(Exp))
    for(i in 3:length(Exp)){
      res[[argList[i-2]]]=deparse(Exp[[i]])
    }
    return(res)
  }
  for(i in 3:length(argName)){
    if(argName[i]!=""){
      res[[argName[i]]]=deparse(Exp[[i]])
    }else{
      for(k in 1:3){
        if(!(argList[k]%in%names(res))){
          res[[argList[k]]]=deparse(Exp[[i]])
          break
        }
      }
    }
  }
  if(res[["drop"]]=="T")
    res[["drop"]]="TRUE"
  res
}

#Remove the useless parenthesis, eg. ((a))
cleanExp<-function(Exp){
  if(is.call(Exp)&&Exp[[1]]=="(")
    return(cleanExp(Exp[[2]]))
  return(Exp)
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
  oldName=toCharacter(oldName)
  if(!is.symbol(newName))
    newName=as.symbol(newName)
  renameList=list(newName)
  names(renameList)=oldName
  if(start<=length(code)){
    for(i in start:length(code)){
      code[[i]]=do.call('substitute', list(code[[i]], renameList))
    }
  }
  return(code)
}