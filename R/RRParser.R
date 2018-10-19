#Level3 compiler
#Functions:
#1.Rename the variable if the variable is redefined
RRcompilerLevel3<-function(level2Exp){
  tmpInd=level2Exp$tmpInd
  parsedExp=level2Exp$Exp
  varList=hash()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(curExp=="{"){
      #code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      var_char=deparse(leftExp)
      if(!is.call(leftExp)){
        if(has.key(var_char,varList)){
          res=getTmpName(tmpInd)
          tmpInd=res$tmpInd
          tmpName=res$tmpName
          parsedExp=renameVarInCode(parsedExp,i+1,var_char,tmpName)
          curExp[[2]]=as.symbol(tmpName)
        }else{
          varList[[var_char]]=var_char
        }
      }
      parsedExp[[i]]=curExp
      next
    }
    if(curExp[[1]]=="for"){
      #Force substitution of the index variable
      res=getTmpName(tmpInd)
      tmpInd=res$tmpInd
      tmpName=res$tmpName
      var_char=deparse(curExp[[2]])
      curExp[[2]]=as.symbol(tmpName)
      loopBody=curExp[[4]]
      loopBody=renameVarInCode(loopBody,1,var_char,tmpName)
      #Check the body code
      res=RRcompilerLevel3(list(tmpInd=tmpInd,Exp=loopBody))
      tmpInd=res$tmpInd
      if(!is.null(res$varList))
        for(j in keys(res$varList)){
          varList[[j]]=j
        }
      curExp[[4]]=res$Exp
      parsedExp[[i]]=curExp
    }
    if(curExp[[1]]=="if"){
      res=RRcompilerLevel3(list(tmpInd=tmpInd,Exp=curExp[[3]]))
      tmpInd=res$tmpInd
      if(!is.null(res$varList))
        for(j in keys(res$varList)){
          varList[[j]]=j
        }
      curExp[[3]]=res$Exp
      parsedExp[[i]]=curExp
    }
  }
  return(list(tmpInd=tmpInd,Exp=parsedExp,varList=varList))
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

#Level2 compiler
#Functions:
#1.For "=" sign: If the left side symbol is subsetted, 
#the right side symbol should be a symbol,
#if not, create a temporary function to replace it
RRcompilerLevel2<-function(level1Exp){
  tmpInd=level1Exp$tmpInd
  parsedExp=level1Exp$Exp
  code=c()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(curExp=="{"){
      #code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      if(is.call(leftExp)&&is.call(rightExp))
      {
        res=createNewVarLevel1(tmpInd,rightExp)
        tmpInd=res$tmpInd
        tmpName=res$TmpName
        code=c(code,res$code)
        curExp[[3]]=as.symbol(tmpName)
      }
    }
    if(curExp[[1]]=="=="){
      for(i in 2:3){
        oneSideExp=curExp[[i]]
        if(is.call(oneSideExp))
        {
          res=createNewVarLevel1(tmpInd,oneSideExp)
          tmpInd=res$tmpInd
          tmpName=res$TmpName
          code=c(code,res$code)
          curExp[[i]]=as.symbol(tmpName)
        }
      }
    }
    if(curExp[[1]]=="for"){
      res=RRcompilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[4]]))
      tmpInd=res$tmpInd
      curExp[[4]]=compressCodeChunk(res$Exp)
    }
    if(curExp[[1]]=="if"){
      res=RRcompilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[3]]))
      tmpInd=res$tmpInd
      curExp[[3]]=compressCodeChunk(res$Exp)
    }
    code=c(code,curExp)
  }
  return(list(tmpInd=tmpInd,Exp=code))
}




#Level 1 compiler
#Functions:
#1.simplify the R code, each line should only have one function call,
#If not, a temporary variable will be created to replace it.
#2.If the code only has a symbol, it will be remove from the function
RRcompilerLevel1<-function(parsedExp,tmpInd=1){
  code=c()
  for(i in 1:length(parsedExp)){
      curExp=parsedExp[[i]]
    if(curExp=="{"){
      #code=c(code,curExp)
      next
    }
    
    if(curExp[[1]]=="="||curExp[[1]]=="=="){
      for(j in 2:3){
        oneSideExp=curExp[[j]]
        if(length(oneSideExp)>=2){
          for(i in seq(2,length(oneSideExp))){
            if(is.call(oneSideExp[[i]])){
              res=createNewVarLevel1(tmpInd,oneSideExp[[i]])
              tmpInd=res$tmpInd
              tmpName=res$TmpName
              code=c(code,res$code)
              curExp[[j]][[i]]=as.symbol(tmpName)
            }
          }
        }
      }
      code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="for"){
      if(is.call(curExp[[3]])){
        loopInd=curExp[[3]]
        if(loopInd[[1]]==":"){
          #Check if the sequance argument is also a function call, if it is,
          #convert it to a variable
          for(i in 2:3){
            if(is.call(loopInd[[i]])){
              res=createNewVarLevel1(tmpInd,loopInd[[i]])
              tmpInd=res$tmpInd
              tmpName=res$TmpName
              code=c(code,res$code)
              curExp[[3]][[i]]=as.symbol(tmpName)
            }
          }
        }
      }
      res=RRcompilerLevel1(curExp[[4]],tmpInd=tmpInd)
      tmpInd=res$tmpInd
      curExp[[4]]=compressCodeChunk(res$Exp)
      code=c(code,curExp)
      next
      }
    if(curExp[[1]]=="if"){
      condition=curExp[[2]]
      if(condition[[1]]!="=="){
        bugLog=addBugInfo(bugLog,"Expect an logic operation sign ==",curExp)
      }else{
        for(i in 2:3){
          if(is.call(condition[[i]])){
            res=createNewVarLevel1(tmpInd,condition[[i]])
            tmpInd=res$tmpInd
            tmpName=res$TmpName
            code=c(code,res$code)
            curExp[[2]][[i]]=as.symbol(tmpName)
          }
        }
      }
      res=RRcompilerLevel1(curExp[[3]],tmpInd=tmpInd)
      tmpInd=res$tmpInd
      curExp[[3]]=compressCodeChunk(res$Exp)
      code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="return"){
      if(is.call(curExp[[2]])){
        res=createNewVarLevel1(tmpInd,curExp[[2]])
        tmpInd=res$tmpInd
        tmpName=res$TmpName
        code=c(code,res$code)
        curExp[[2]]=as.symbol(tmpName)
      }
      code=c(code,curExp)
      next
      }
   stop("Unrecognized code: ",deparse(curExp))
  }
  return(list(tmpInd=tmpInd,Exp=code))
}

compressCodeChunk<-function(Exp){
  code=c()
  if(length(Exp)>1){
    for(i in 1:length(Exp))
      code=c(code,deparse(Exp[[i]]))
  }else{
    code=deparse(Exp)
  }
  code=c("{",code,"}")
  code=paste0(code,collapse = "\n")
  return(parse(text=code)[[1]])
}
  
#parsedExp=parse(text="f(g(),a)")[[1]]
#Create a new variable to represent a function call
createNewVarLevel1<-function(tmpInd,parsedExp){
  res=getTmpName(tmpInd)
  tmpInd=res$tmpInd
  curTmpName=res$tmpName
  curCode=c()
  if(length(parsedExp)>1){
    for(i in seq(2,length(parsedExp))){
      #If the argument is also a function call
      curArg=parsedExp[[i]]
      if(is.call(curArg)){
        res=createNewVarLevel1(tmpInd,curArg)
        tmpInd=res$tmpInd
        tmpName=res$TmpName
        #change the argument to a parameter
        parsedExp[[i]]=as.symbol(tmpName)
        curCode=c(curCode,res$code)
      }
    }
  }
  curCode=c(
    curCode,
    parse(text=paste0(curTmpName,"=",deparse(parsedExp,backtick=F)))
  )
  return(list(tmpInd=tmpInd,code=curCode,TmpName=curTmpName))
}


getTmpName<-function(tmpInd){
  return(list(tmpInd=tmpInd+1,tmpName=paste0("opencl_tmp_",tmpInd)))
}

#convert a function to an expression
funcToExp<-function(f){
  charExp=deparse(f)
  parsedExp=parse(text=charExp)[[1]]
  args=parsedExp[[2]]
  code=parsedExp[[3]]
  return(list(args=args,code=code))
}

printExp<-function(Exp){
  for(i in Exp){
    message(deparse(i))
  }
}
