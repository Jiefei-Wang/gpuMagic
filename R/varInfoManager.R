
getEmpVarInfoTbl<-function(){
  varInfo=list()
  varInfo$profile=hash()
  #current version
  varInfo$varVersion=hash()
  varInfo
}

copyVarInfoTbl <- function(varInfo, resetVersion = T) {
  newInfo = varInfo
  for (i in names(varInfo)) {
    if (is.hash(varInfo[[i]]))
      newInfo[[i]] = copy(newInfo[[i]])
    else
      newInfo[[i]] = newInfo[[i]]
  }
  if (resetVersion) {
    for (i in keys(newInfo$varVersion)) {
      newInfo$varVersion[[i]] = 1
    }
  }
  newInfo
}

getEmpyTable<-function(type=""){
  tbl=data.frame(var="NA",dataType=T_matrix,precisionType=GPUVar$default_float,size1="NA",size2="NA",value="NA",
                 location="global",shared=FALSE,version=1,
                 address="NA",compileSize1=FALSE,compileSize2=FALSE,compileValue=FALSE,transpose=FALSE,
                 require=FALSE,constVal=FALSE,constDef=FALSE,initialization=TRUE,
                 isRef=FALSE,ref="",
                 isSeq=FALSE,seq="",
                 
                 stringsAsFactors=FALSE)
  if(type==T_scale){
    tbl$dataType=T_scale
    tbl$compileSize1=TRUE
    tbl$compileSize2=TRUE
    tbl$size1=1
    tbl$size2=1
    tbl$location="local"
  }
  tbl
}


#Convert key to a hash set
#If value is provided, it is a hash map
toHash<-function(key,value=NULL){
  if(is.null(value))
    value=rep(0,length(key))
  names(value)=key
  hash(value)
}

primaryProp=c("dataType","precisionType","address","shared","location",
                  "require","constVal","constDef","initialization",
                  "isSeq","seq","isRef","ref")
primaryPropHash=toHash(primaryProp)


isPrimary<-function(x){
  has.key(x,primaryPropHash)
}

#Check if a variable is in the table
hasVar<-function(varInfo,varName,version="auto"){
  varName=toCharacter(varName)
  if(length(version)==0) return(FALSE)
  if(version=="auto"){
    version=0
  }
  var_char=paste0(varName,"+",version)
  return(has.key(var_char,varInfo$profile))
}



#Get the variable profile
getVarInfo<-function(varInfo,varName,version="auto"){
  var_char=toCharacter(varName)
  if(version=="auto")
    version=as.numeric(varInfo$varVersion[[var_char]])
  
  
  #Check if the symbol does not exist in the table
  if(!hasVar(varInfo,var_char,version))
    stop(paste0("The variable is not found: ",var_char))
  
  varDef_char=paste0(var_char,"+",0)
  varCur_char=paste0(var_char,"+",version)
  
  varDef_tbl=varInfo$profile[[varDef_char]]
  varCur_tbl=varInfo$profile[[varCur_char]]
  
  varCur_tbl[1,primaryProp]=varDef_tbl[1,primaryProp]
  
  varCur_tbl
}



#Set or add the variable info without any check
#The function will take care of version 0
setVarInfo_hidden<-function(varInfo,info){
  var_char=info$var
  version=info$version
  varDef_char=paste0(var_char,"+",0)
  varCur_char=paste0(var_char,"+",version)
  if(version!=0){
    if(hasVar(varInfo,info$var,0)){
      varDef_tbl=varInfo$profile[[varDef_char]]
      varDef_tbl[1,primaryProp]=info[1,primaryProp]
      varInfo$profile[[varDef_char]]=varDef_tbl
    }else{
      if(version!=1)
        stop("The variable definition is not found and the current version is not 1")
      varDef_tbl=info
      varDef_tbl$version=0
      varInfo$profile[[varDef_char]]=varDef_tbl
    }
    info$version=version
    varInfo$profile[[varCur_char]]=info
    if(has.key(var_char,varInfo$varVersion)){
      varInfo$varVersion[[var_char]]=max(varInfo$varVersion[[var_char]],version)
    }else{
      varInfo$varVersion[[var_char]]=version
    }
  }else{
    varInfo$profile[[varDef_char]]=info
  }
  return(varInfo)
}

#Set the variable info
#If the variable does not exist, an error will be given
setVarInfo<-function(varInfo,newInfo){
  #Check if the symbol does not exist in the table
  if(!hasVar(varInfo,newInfo$var,newInfo$version))
    stop(paste0("The given variable is not found: ",var_char))
  
  setVarInfo_hidden(varInfo,newInfo)
}

#Add a variable in the table
#If the variable with a give version has exist in the table, an error will be given
addVarInfo<-function(varInfo,newInfo){
  #Check if the info already exist
  if(hasVar(varInfo,newInfo$var,newInfo$version))
    stop(paste0("The given variable is already in the table: ",newInfo$var))
  
  
  setVarInfo_hidden(varInfo,newInfo)
}



getVarProperty<-function(varInfo,varName,property,version="auto"){
  var_char=toCharacter(varName)
  
  if (!hasVar(varInfo,var_char,version=version))
    stop("The given variable is not found: ", var_char)
  if (version == "auto") {
    version = as.numeric(varInfo$varVersion[[var_char]])
  }
  
  varCur_char=paste0(var_char,"+",version)
  var_tbl=varInfo$profile[[varCur_char]]
  value=var_tbl[[property]]
  return(value)
}


setVarProperty<-function(varInfo,varName,property,value,version="auto"){
  varName=toCharacter(varName)
  
  if (!hasVar(varInfo,varName,version=version))
    stop("The given variable is not found: ", varName)
  if (version == "auto") {
    version = as.numeric(varInfo$varVersion[[varName]])
  }
  
  varCur_char=paste0(var_char,"+",version)
  var_tbl=varInfo$profile[[varCur_char]]
  var_tbl[[property]]=value
  varInfo$profile[[var_char]]=var_tbl
  
  if(isPrimary(property)){
    varDef_char=paste0(var_char,"+",0)
    var_tbl=varInfo$profile[[varDef_char]]
    var_tbl[[property]]=value
    varInfo$profile[[var_char]]=var_tbl
  }
  
  return(varInfo)
}



printVarInfo<-function(varInfo,simplify=TRUE,printDef=FALSE){
  if(!is.null(varInfo[["varInfo"]]))
    varInfo=varInfo[["varInfo"]]
  simplifyTbl=c("var","dataType","precisionType","size1","size2","value","ref","seq","version")
  info=c()
  for(i in keys(varInfo$profile)){
    var_tbl=varInfo$profile[[i]]
    if(!printDef&&var_tbl$version==0)
      next
    if(simplify)
      var_tbl=var_tbl[1,simplifyTbl]
    info=rbind(info,var_tbl)
  }
  info
}

