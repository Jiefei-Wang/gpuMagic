#1.Rename the first function argument and add the looped variable in a code format
#2.special treatment for the break and next
codePreprocessing<-function(codeMetaInfo){
  GPUVar$resetTmpCount()
  loopVar=names(codeMetaInfo$parms)[[1]]
  names(codeMetaInfo$parms)[[1]]=GPUVar$gpu_loop_data
  #insert the preserved data reading code
  readDataExp=parse(text=paste0(loopVar,"=",GPUVar$gpu_loop_data,"[",GPUVar$gpu_global_id,"+1]"))
  Exp=c(readDataExp,codeMetaInfo$Exp)
  #Exp=renameControlCode(Exp)
  
  codeMetaInfo$Exp=Exp
  codeMetaInfo
}
#Level 1 compiler
#Functions:
#1.simplify the R code, each line should only have one function call,
#If not, a temporary variable will be created to replace it.
#2.If the code only has a symbol and the symbol is not recognized, it will be removed
RParser1<-function(codeMetaInfo){
  codeMetaInfo1=parserFrame(RLevel1_parserFunc,RLevel1_checkFunc,
                            RLevel1_updateFunc,codeMetaInfo)
  codeMetaInfo1
}


RLevel1_parserFunc<-function(level,codeMetaInfo,curExp){
  result=list()
  
  code_char=deparse(curExp)[1]
  #If the function is the opencl code, pass it
  if(substr(code_char,1,nchar(GPUVar$openclCode))==GPUVar$openclCode||
     substr(code_char,1,nchar(GPUVar$openclFuncCall))==GPUVar$openclFuncCall){
    result$Exp=curExp
    return(result)
  }
  
  if(is.call(curExp)){
    if(curExp[[1]]=="="||curExp[[1]]=="=="){
      result=simplifySingleCode(curExp)
      return(result)
    }
    #General strategy for all functions call that do not appear above. E.g. f(g())
    result=simplifyElementOp(curExp,wholeReplace=TRUE,useElementOp=FALSE)
    return(result)
  }else{
    result$Exp=curExp
    return(result)
  }
  
  #Default return value
  stop("You should not be here!")
}

RLevel1_checkFunc<-function(curExp){
  return(TRUE)
}

RLevel1_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result
}


#1. extract the loop variable and if condition and create a new variable for them
RParser2<-function(codeMetaInfo1){
  codeMetaInfo2=codeMetaInfo1
  parsedExp=extract_for_if_Var(codeMetaInfo2$Exp)
  codeMetaInfo2$Exp=parsedExp
  
  codeMetaInfo2
}









