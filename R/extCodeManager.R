
addVarDefInExt<-function(extCode,precision,varName,varDef){
  extCode=rbind(extCode,
                data.frame(precision=precision,varName=varName,varDef=varDef,additionalCode=NA,stringsAsFactors = FALSE))
  extCode
}
addAdditionalCodeInExt<-function(extCode,additionalCode){
  extCode=rbind(extCode,
                data.frame(precision=NA,varName=NA,varDef=NA,additionalCode=additionalCode,stringsAsFactors = FALSE))
  extCode
}

finalizeExtCode<-function(extCode){
  code=c()
  if(!is.null(extCode)){
    for(i in seq_len(nrow(extCode))){
      curCode=extCode[i,]
      if(is.na(curCode$additionalCode)){
        code=rbind(code,paste0(curCode$precision," ",curCode$varName,"=",curCode$varDef,";"))
      }else{
        code=rbind(code,curCode$additionalCode)
      }
    }
  }
  code
}

getVarFromExtCode<-function(extCode,precision,varDef){
  ind=which(precision==extCode$precision&varDef==extCode$varDef)
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
