#==============================parser 1==========================

#parsedExp=parse(text="(tmp + tmp1) * tmp")[[1]]
#Create a new variable to represent a function call
createNewVar<-function(parsedExp){
  parsedExp=cleanExp(parsedExp)
  varName=GPUVar$getTmpVar()
  curCode=c()
  if(length(parsedExp)>1){
    for(i in seq(2,length(parsedExp))){
      #If the argument is also a function call
      curArg=parsedExp[[i]]
      curArg_char=deparse(parsedExp[[i]])
      if(curArg_char!=""&&is.call(curArg)){
        res=createNewVar(curArg)
        #change the argument to a parameter
        parsedExp[[i]]=as.symbol(res$varName)
        curCode=c(curCode,res$code)
      }
    }
  }
  
  
  if(parsedExp[[1]]=="["){
    subsetArgs=matchBracketFunc(parsedExp)
    if(is.null(subsetArgs$j)){
      replaceCode=parse(text=paste0(varName,"=subRef(",parsedExp[[2]],",",subsetArgs$i,")"))[[1]]
    }else{
      replaceCode=parse(text=paste0(varName,"=subRef(",parsedExp[[2]],",",subsetArgs$i,",",subsetArgs$j,")"))[[1]]
    }
  }else{
    replaceCode=parse(text=paste0(varName,"=",deparse(parsedExp)))[[1]]
  }
  
  curCode=c(
    curCode,
    replaceCode
  )
  #tmpMeta$varName=curName
  return(list(varName=varName,code=curCode))
}

#Exp=quote(a[1,])
#This function will return a list of the arguments of the [] function, the empty argments are expressed in character
#example: 
#a[1]     ==>i=1,drop=TRUE
#a[1,]    ==>i=1,b="",drop=TRUE
#Exp=quote(a[])
matchBracketFunc<-function(Exp){
  res=list(drop=TRUE)
  argName=names(Exp)
  argList=c("i","j","drop")
  
  if(is.null(argName)){
    if(length(Exp)<3)
      stop("Unexpected expression:", deparse(Exp))
    for(i in 3:length(Exp)){
      res[[argList[i-2]]]=Exp[[i]]
    }
    for(i in 1:length(res)){
      if(deparse(res[[i]])=="")
        res[[i]]=""
    }
    return(res)
  }
  
  for(i in 3:length(argName)){
    if(argName[i]!=""){
      res[[argName[i]]]=Exp[[i]]
    }else{
      for(k in 1:3){
        if(!(argList[k]%in%names(res))){
          res[[argList[k]]]=Exp[[i]]
          break
        }
      }
    }
  }
  if(res[["drop"]]=="T")
    res[["drop"]]="TRUE"
  
  for(i in 1:length(res)){
    if(deparse(res[[i]])=="")
      res[[i]]=""
  }
  res
}

#Remove the useless parenthesis, eg. ((a))
cleanExp<-function(Exp){
  if(is.call(Exp)&&Exp[[1]]=="(")
    return(cleanExp(Exp[[2]]))
  return(Exp)
}


compressCodeChunk<-function(Exp){
  if(is.symbol(Exp)||isNumeric(Exp)||Exp[[1]]!="{"){
    Exp=as.call(c(as.symbol("{"),Exp))
  }else{
    Exp=as.call(Exp)
  }
  return(Exp)
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


#================parser 2=================


extract_for_if_Var<-function(parsedExp){
  code=c()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(!is.call(curExp)){
      code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="for"){
      #Force substitution of the index variable
      index_var=curExp[[2]]
      index_newVar=GPUVar$getTmpVar()
      index_def_code=paste0(index_newVar,"=gNumber(precision=\"",GPUVar$default_index_type,"\",constDef=TRUE)")
      
      loopNum=curExp[[3]]
      if(is.symbol(loopNum)){
        loopNumVar=deparse(loopNum)
        loopNum_def_Code=NULL
      }else{
        loopNumVar=GPUVar$getTmpVar()
        loopNum_def_Code=paste0(loopNumVar,"=",deparse(loopNum))
      }
      
      loopNumCountvar=GPUVar$getTmpVar()
      loopNumCountvar_def_code=paste0(loopNumCountvar,"=length(",loopNumVar,")")
      
      
      #assign the value to the looped variable
      index_var_code=paste0(deparse(index_var),"=",loopNumVar,"[",index_newVar,"]")
      
      
      
      loopNumExp=parse(text=paste0("1:",loopNumCountvar))[[1]]
      index_def_code=parse(text=index_def_code)[[1]]
      loopNum_def_Code=parse(text=loopNum_def_Code)[[1]]
      loopNumCountvar_def_code=parse(text=loopNumCountvar_def_code)[[1]]
      index_var_code=parse(text=index_var_code)[[1]]
      
      code=c(code,index_def_code,loopNum_def_Code,loopNumCountvar_def_code)
      
      curExp[[2]]=parse(text=index_newVar)[[1]]
      curExp[[3]]=loopNumExp
      
      loopBody=curExp[[4]]
      loopBody_new=extract_for_if_Var(loopBody)
      
      loopBody_new=c(loopBody_new[1],index_var_code,loopBody_new[-1])
      curExp[[4]]=as.call(loopBody_new)
      
      code=c(code,curExp)
      next
    }
    if(curExp[[1]]=="if"){
      condition=curExp[[2]]
      if(!is.symbol(condition)){
        conditionVar=GPUVar$getTmpVar()
        conditionVar_def_code=paste0(conditionVar,"=",deparse(condition))
        
        conditionVar_def_code=parse(text=conditionVar_def_code)[[1]]
        
        code=c(code,conditionVar_def_code)
        curExp[[2]]=as.symbol(conditionVar)
        
      }
      code=c(code,curExp)
      next
    }
    
    code=c(code,curExp)
  }
  return(code)
}


