#order=1: optimization code
#order=0: extra code
addVarDefInExt<-function(extCode,precision,varName,varDef,order=1){
  extCode=rbind(extCode,
                data.frame(precision=precision,varName=varName,varDef=varDef,additionalCode=NA,stringsAsFactors = FALSE,order=order))
  extCode
}
addAdditionalCodeInExt<-function(extCode,additionalCode,order=1){
  extCode=rbind(extCode,
                data.frame(precision=NA,varName=NA,varDef=NA,additionalCode=additionalCode,stringsAsFactors = FALSE,order=order))
  extCode
}
removeAdditionalCodeInExt<-function(extCode){
  extCode=extCode[is.na(extCode$additionalCode),]
  extCode
}
removeVarDefInExt<-function(extCode){
  extCode=extCode[!is.na(extCode$additionalCode),]
  extCode
}

combineExtCode<-function(...){
  parms=list(...)
  extCode=parms[[1]]
  for(i in parms[-1]){
    extCode=rbind(extCode,i)
  }
  extCode
}

finalizeExtCode_hidden<-function(curCode){
  if(is.na(curCode$additionalCode)){
    return(paste0(curCode$precision," ",curCode$varName,"=",curCode$varDef,";"))
  }else{
    return(curCode$additionalCode)
  }
  
}
#optCode: The optimization code that can be placed far away from the current code
#extCode: the extra code that must be put right before the current code
finalizeExtCode<-function(extCode){
  optCode=c()
  extraCode=c()
  if(!is.null(extCode)){
    for(i in seq_len(nrow(extCode))){
      curCode=extCode[i,]
      if(curCode$order==1){
        optCode=rbind(optCode,finalizeExtCode_hidden(curCode))
      }
      if(curCode$order==0){
        extraCode=rbind(extraCode,finalizeExtCode_hidden(curCode))
      }
    }
  }
  list(optCode=optCode,extraCode=extraCode)
}
isVarExist<-function(extCode,precision,varDef,order=1){
  ind=which(precision==extCode$precision&varDef==extCode$varDef&extCode$order==order)
  return(length(ind)!=0)
}


getVarFromExtCode<-function(extCode,precision,varDef,order=1){
  ind=which(precision==extCode$precision&varDef==extCode$varDef&extCode$order==order)
  if(length(ind)>1){
    warning("redundant code has been found!")
    ind=ind[1]
  }
  if(length(ind)==1){
    varName=extCode[ind,"varName"]
  }else{
    varName=GPUVar$getTmpVar()
    extCode=addVarDefInExt(extCode,precision,varName,varDef)
  }
  
  return(list(var=varName,extCode=extCode))
}

