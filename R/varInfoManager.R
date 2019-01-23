
getEmpVarInfoTbl<-function(){
  varInfo=list()
  #var+version: profile
  varInfo$profile=hash()
  #current version
  #Var: version
  varInfo$varVersion=hash()
  varInfo$printAll=function(){
    callFunc=sys.call()
    curInfo=eval(callFunc[[1]][[2]],envir=globalenv())
    print.varInfo(curInfo,simplify=FALSE)
  }
  varInfo$PrintDef=function(){
    callFunc=sys.call()
    curInfo=eval(callFunc[[1]][[2]],envir=globalenv())
    print.varInfo(curInfo,simplify=FALSE,printDef = TRUE)
  }
  
  structure(varInfo,class="varInfo")
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

#isRef=FALSE,ref="",
#isSeq=FALSE,seq="",redirect="NA"

getEmpyTable<-function(type=""){
  tbl=data.frame(
    #variable documentation
    var="NA",dataType=T_matrix,precisionType=GPUVar$default_float,
    size1="NA",size2="NA",value="NA",transpose=FALSE,
    version=1,
    #Physical storage information
    address="NA",designSize1="NA",designSize2="NA",totalSize="NA",
    #How does a matrix align in the momery
    storageMode="column",
    location="global",shared=FALSE,
    #compilation property
    require=FALSE,constVal=FALSE,constDef=FALSE,initialization=TRUE,
    #special type: ref,seq
    isSpecial=FALSE,specialType="NA",specialContent="NA",
    redirect="NA",
    stringsAsFactors=FALSE)
  if(type==T_scale){
    tbl$dataType=T_scale
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

primaryProp=c("dataType","precisionType","address","designSize1","designSize2","totalSize",
              "shared","location",
              "require","constVal","constDef","initialization","redirect")
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
  
  varCur_tbl$isSeq=varCur_tbl$specialType=="seq"
  varCur_tbl$isRef=varCur_tbl$specialType=="ref"
  
  
  varCur_tbl
}


getAllVars<-function(varInfo){
  keys(varInfo$varVersion)
}


#Set or add the variable info without any check
#The function will take care of version 0
setVarInfo_hidden<-function(varInfo,info){
  initVarDef<-function(info){
    info$version=0
    if(isNA(info$designSize1))
      info$designSize1=info$size1
    if(isNA(info$designSize2))
      info$designSize2=info$size2
    info$totalSize=Simplify(paste0("(",info$designSize1,")*(",info$designSize2,")"))
    return(info)
  }
  var_char=info$var
  version=info$version
  varDef_char=paste0(var_char,"+",0)
  varCur_char=paste0(var_char,"+",version)
  if(version!=0){
    #Check if the variable information has been in the table
    if(hasVar(varInfo,info$var,0)){
      varDef_tbl=varInfo$profile[[varDef_char]]
      varDef_tbl[1,primaryProp]=info[1,primaryProp]
      varInfo$profile[[varDef_char]]=varDef_tbl
    }else{
      if(version!=1)
        stop("The variable definition is not found and the current version is not 1")
      
      varDef_tbl=initVarDef(info)
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
    varDef_tbl=initVarDef(info)
    varInfo$profile[[varDef_char]]=varDef_tbl
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
  
  vardef_char=paste0(var_char,"+",0)
  varCur_char=paste0(var_char,"+",version)
  if(isPrimary(property)){
    var_tbl=varInfo$profile[[vardef_char]]
  }else{
    var_tbl=varInfo$profile[[varCur_char]]
  }
  value=switch(property,
         "isSeq"=var_tbl$specialType=="seq",
         "isRef"=var_tbl$specialType=="ref",
         var_tbl[[property]]
         )
  #value=var_tbl[[property]]
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



print.varInfo<-function(varInfo,simplify=TRUE,printDef=FALSE){
  simplifyTbl=c("var","dataType","precisionType","size1","size2",
                "value","specialType","specialContent","version","address")
  info=c()
  for(i in keys(varInfo$profile)){
    var_tbl=varInfo$profile[[i]]
    varName=var_tbl$var
    varVersion=var_tbl$version
    var_tbl=getVarInfo(varInfo,varName,varVersion)
    if(!printDef&&varVersion==0)
      next
    if(simplify)
      var_tbl=var_tbl[1,simplifyTbl]
    info=rbind(info,var_tbl)
  }
  print(info)
}

