
getEmpVarInfoTbl<-function(){
  varInfo=list()
  #var+version: profile
  varInfo$profile=hash()
  #current version
  #Var: version
  varInfo$varVersion=hash()
  varInfo$memPool=createMemPool()
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

copyVarInfoTbl <- function(varInfo, resetVersion = TRUE) {
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
  #variable documentation
  tbl=data.frame(
    var="NA",dataType=T_matrix,precisionType=GPUVar$default_float,
    size1="NA",size2="NA",value="NA",transpose=FALSE,
    version=1,
    #Physical storage information
    address="NA",designSize="NA",
    #How does a matrix align in the momery: mixed or single
    storageMode="mixed",
    location="global",shared=FALSE,
    #compilation property
    require=FALSE,constVal=FALSE,constDef=FALSE,
    #Specify whether the address will be initialized
    initial_ad=TRUE,
    #special type: ref,seq
    isSpecial=FALSE,specialType="NA",specialContent="NA",
    redirect="NA",isPointer=NA,
    stringsAsFactors=FALSE)
  
  if(type==T_scale){
    tbl$dataType=T_scale
    tbl$size1=1
    tbl$size2=1
    tbl$location="local"
  }
  
  attr(tbl,"infoType")="singleInfo"
  tbl
}


allProp=names(getEmpyTable())
primaryProp=c("dataType","precisionType","address","designSize",
              "storageMode","shared","location",
              "require","constVal","constDef","initial_ad","redirect","isPointer")



isPrimary<-function(x){
  x%in%primaryProp
}

#Check if a variable is in the table
#x: varInfo
hasVar.varInfo<-function(x,varName,version="auto"){
  varName=toCharacter(varName)
  if(length(version)==0) return(FALSE)
  if(version=="auto"){
    version=0
  }
  var_char=paste0(varName,"+",version)
  return(has.key(var_char,x$profile))
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
  
  varCur_tbl$isSeq=(varCur_tbl$specialType=="seq")
  varCur_tbl$isRef=(varCur_tbl$specialType=="ref")
  
  attr(varCur_tbl,"infoType")="singleInfo"
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
    if(isNA(info$designSize))
      info$designSize=Simplify(paste0("(",info$size1,")*(",info$size2,")"))
    
    if(info$shared)
      info$storageMode="single"
    return(info)
  }
  info$isSeq=NULL
  info$isRef=NULL
  if(sum(!names(info)%in%allProp)!=0){
    stop("Incorrect variable info")
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
    }else{
      if(version!=1)
        stop("The variable definition is not found and the current version is not 1")
      
      varDef_tbl=initVarDef(info)
      #Find if the memory pool has the free variable
      res=def_var(varInfo,varDef_tbl)
      varInfo=res$varInfo
      varDef_tbl=res$curInfo
    }
    
    varInfo$profile[[varDef_char]]=varDef_tbl
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
  if(attr(newInfo,"infoType")!="singleInfo")
    warning("The info class is incorrent, this should be a bug in the package")
  #Check if the symbol does not exist in the table
  if(!hasVar(varInfo,newInfo$var,newInfo$version))
    stop(paste0("The given variable is not found: ",newInfo$var))
  
  setVarInfo_hidden(varInfo,newInfo)
}

#Add a variable in the table
#If the variable with a give version has exist in the table, an error will be given
addVarInfo<-function(varInfo,newInfo){
  if(attr(newInfo,"infoType")!="singleInfo")
    warning("The info class is incorrent, this should be a bug in the package")
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



#' @rdname printFunctions
#' @method print varInfo
#' @param simplify Specify whether only the important properties should be printed
#' @param printDef Whether the variable definition should be printed(version=0)
#' @export
print.varInfo<-function(x,simplify=TRUE,printDef=FALSE,...){
  simplifyTbl=c("var","dataType","precisionType","size1","size2",
                "value","specialType","specialContent","version","address")
  info=c()
  for(i in keys(x$profile)){
    var_tbl=x$profile[[i]]
    varName=var_tbl$var
    varVersion=var_tbl$version
    var_tbl=getVarInfo(x,varName,varVersion)
    if(!printDef&&varVersion==0)
      next
    if(simplify)
      var_tbl=var_tbl[1,simplifyTbl]
    info=rbind(info,var_tbl)
  }
  print(info)
}



release_var<-function(varInfo,varName){
  varName=toCharacter(varName)
  curInfo=getVarInfo(varInfo,varName)
  hasData=(!curInfo$isSpecial)||(curInfo$isSpecial&&curInfo$specialType%in%c("seq"))
  if(hasData){
    if(curInfo$redirect=="NA"){
      varInfo$memPool=addVar(
        varInfo$memPool,varName,
        curInfo$precisionType,curInfo$designSize,curInfo$location,
        curInfo$shared,curInfo$isPointer,curInfo$specialType)
      varInfo$memPool=markFree(varInfo$memPool,varName)
    }else{
      redirectVar=curInfo$redirect
      return(release_var(varInfo,redirectVar))
    }
  }
  return(varInfo)
}

def_var<-function(varInfo,curInfo){
  varName=curInfo$var
  hasData=(!curInfo$isSpecial)||(curInfo$isSpecial&&curInfo$specialType%in%c("seq"))
  if(hasData){
    if(curInfo$redirect=="NA"){
      rediectVar=findFreeVar(
        varInfo$memPool,
        curInfo$precisionType,curInfo$designSize,curInfo$location,
        curInfo$shared,curInfo$isPointer,curInfo$specialType)
      if(!is.null(rediectVar)){
        curInfo$redirect=rediectVar
        curInfo$designSize=0
        varInfo$memPool=markUsed(varInfo$memPool,rediectVar)
      }
    }
  }
  return(list(varInfo=varInfo,curInfo=curInfo))
}



createMemPool<-function(){
  memPool=list()
  memPool$obj_isUsed=c()
  memPool$obj=data.frame(
    var=character(),precision=character(),length=character(),
    location=character(),shared=logical(),isPointer=logical(),
    specialType=character(),stringsAsFactors = FALSE)
  
  structure(memPool,class="memPool")
}
extractVars.memPool<-function(x){
  return(x$obj$var)
}
markFree<-function(memPool,varName){
  ind=which(extractVars(memPool)==varName)
  if(length(ind)==1){
    memPool$obj_isUsed[ind]=FALSE
  }
  memPool
}
markUsed<-function(memPool,varName){
  ind=which(extractVars(memPool)==varName)
  if(length(ind)==1){
    memPool$obj_isUsed[ind]=TRUE
  }else{
    stop("The variable name is not in the table:", varName)
  }
  memPool
}
addVar<-function(memPool,varName,precision,len,location,shared=FALSE,isPointer=NA,specialType="NA"){
  if(hasVar(memPool,varName,precision,len,location,shared,isPointer,specialType)){
    return(memPool)
  }
  curInfo=createMemPoolInfo(varName,precision,len,location,shared,isPointer,specialType)
  memPool$obj=rbind(memPool$obj,curInfo)
  memPool$obj_isUsed=c(memPool$obj_isUsed,FALSE)
  memPool
}
hasVar.memPool<-function(x,varName,precision,len,location,shared=FALSE,isPointer=NA,specialType="NA"){
  if(is.na(isPointer)){
    ind=which(names(x$obj)=="isPointer")
  }
  curInfo=createMemPoolInfo(varName,precision,len,location,shared,isPointer,specialType)
  res=apply(x$obj[,-ind],1, function(x,y)sum(x!=y)==0,y=curInfo[,-ind])
  sum(res)>0
}
createMemPoolInfo<-function(varName,precision,len,location,shared=FALSE,isPointer=NA,specialType="NA"){
  data.frame(
    var=varName,precision=precision,length=len,
    location=location,shared=shared,isPointer=isPointer,specialType=specialType,stringsAsFactors = FALSE)
}

findFreeVar<-function(memPool,precision,len,location,shared=FALSE,isPointer=NA,specialType="NA"){
  if(sum(memPool$obj_isUsed)==length(memPool$obj_isUsed))return(NULL)
  if(is.na(isPointer)){
    ind=which(names(memPool$obj)=="isPointer")
  }
  ind=c(ind,which(names(memPool$obj)=="var"))
  poolVarInfo=memPool$obj[!memPool$obj_isUsed,]
  curInfo=createMemPoolInfo("NA",precision,len,location,shared,isPointer,specialType)
  res=apply(poolVarInfo[,-ind],1, function(x,y)sum(x!=y)==0,y=curInfo[,-ind])
  res1=which(res)
  if(length(res1)>0){
    return(poolVarInfo[res1[1],]$var)
  }else{
    return(NULL)
  }
}


