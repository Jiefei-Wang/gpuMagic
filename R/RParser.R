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
      leftExp=cleanExp(curExp[[2]])
      rightExp=cleanExp(curExp[[3]])
      #Left expression
      #Check if it is a function call
      if(is.call(leftExp)){
        #If the left expression is a matrix subset, replace it with a new variable
        #Otherwise only replace the function argument if needed
        if(leftExp[[1]]=="["){
          res=createNewVar(leftExp)
          result$extCode=c(result$extCode,res$code)
          leftExp=as.symbol(res$varName)
        }else{
          #If the function has an argument which also is a function, replace it with a variable 
          if(length(leftExp)>=2){
            for(i in seq(2,length(leftExp))){
              if(is.call(leftExp[[i]])){
                res=createNewVar(leftExp[[i]])
                result$extCode=c(result$extCode,res$code)
                leftExp[[i]]=as.symbol(res$varName)
              }
            }
          }
        }
      }
      #For the right expression
      #If the function has an argument which also is a function, replace it with a variable 
      if(length(rightExp)>=2){
        for(i in seq(2,length(rightExp))){
          if(is.call(rightExp[[i]])){
            res=createNewVar(rightExp[[i]])
            result$extCode=c(result$extCode,res$code)
            rightExp[[i]]=as.symbol(res$varName)
          }
        }
      }
      curExp[[2]]=leftExp
      curExp[[3]]=rightExp
      result$Exp=curExp
      return(result)
    }
    #General strategy for all functions call that do not appear above. E.g. f(g())
    if(length(curExp)>1){
      for(i in 2:length(curExp)){
        if(deparse(curExp[[i]])!=""&&is.call(curExp[[i]])){
          res=createNewVar(curExp[[i]])
          result$extCode=c(result$extCode,res$code)
          curExp[[i]]=as.symbol(res$varName)
        }
      }
      result$Exp=curExp
      return(result)
    }
  }else{
    result$Exp=NULL
    return(result)
  }
  
  
  #Default return value
  stop("You should not be here!")
  result$Exp=curExp
  return(result)
}

RLevel1_checkFunc<-function(curExp){
  return(TRUE)
}

RLevel1_updateFunc<-function(type,level,codeMetaInfo,parsedExp,code,i,res){
  result=general_updateFunc(codeMetaInfo,parsedExp,code)
  result
}












