
getEmpVarInfoTbl<-function(){
  varInfo=list()
  varInfo$profile=data.frame()
  varInfo$varVersion=hash()
  varInfo$varIndex=hash()
  varInfo$optProfile=hash()
  varInfo
}

copyVarInfoTbl<-function(varInfo){
  newInfo=varInfo
  newInfo$profile=varInfo$profile
  newInfo$varVersion=copy(varInfo$varVersion)
  newInfo$varIndex=copy(varInfo$varIndex)
  newInfo$optProfile=copy(varInfo$optProfile)
  for(i in keys(varInfo$varVersion)){
    newInfo$varVersion[[i]]=1
  }
  newInfo
}

getEmpyTable<-function(type=""){
  tbl=data.frame(var="NA",dataType=T_matrix,precisionType=gpuMagic.option$getDefaultFloat(),size1="NA",size2="NA",value="NA",
                 shared=FALSE,location="global",version=1,
                 address="NA",compileSize1=FALSE,compileSize2=FALSE,compileValue=FALSE,transpose=FALSE,
                 require=FALSE,constVal=FALSE,constDef=FALSE,lazyRef=FALSE,ref="",initialization=TRUE,changed=FALSE,used=FALSE,stringsAsFactors=FALSE)
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

getMandatoryVar<-function(){
  c("var","dataType","precisionType","size1","size2","value","shared","location","version")
}
getOptVar<-function(){
  c("address","compileSize1","compileSize2","compileValue","transpose","require",
    "constVal","constDef","lazyRef","ref","initialization","changed","used")
}
getTableVarProperty<-function(){
  mandatoryVar=getMandatoryVar()
  optVar=getOptVar()
  varName=c(mandatoryVar,optVar)
  tbl=matrix(c(rep("mandatory",length(mandatoryVar)),rep("optional",length(optVar))),1)
  tbl=as.data.frame(tbl)
  colnames(tbl)=varName
  tbl
}

#Check if a variable is in the table
hasVar<-function(varInfo,varName,version="all"){
  var_char=toCharacter(varName)
  if(version=="all")
    return(has.key(var_char,varInfo$varVersion))
  if(is.numeric(version)){
    var_char=paste0(var_char,"+",version)
    return(has.key(var_char,varInfo$varIndex))
  }
  stop("An error has been occured")
}



#Get the variable profile
getVarInfo<-function(varInfo,varName,version="auto"){
  var_char=toCharacter(varName)
  if(version=="auto")
    version=as.numeric(varInfo$varVersion[[var_char]])
  
  
  #Check if the symbol does not exist in the table
  if(!hasVar(varInfo,var_char,version))
    stop(paste0("The given variable is not found: ",var_char))
  
  var_char=paste0(var_char,"+",version)
  var_ind=varInfo$varIndex[[var_char]]
  var_data=varInfo$profile[var_ind,,drop=F]
  var_Tbl=getEmpyTable()
  var_Tbl[1,colnames(var_data)]=var_data[1,colnames(var_data)]
  var_opt_data=varInfo$optProfile[[var_char]]
  if(!is.null(var_opt_data)){
    for(i in colnames(var_opt_data)){
      var_Tbl[1,i]=var_opt_data[1,i]
    }
  }
  
  var_Tbl
}


#Set the variable info
#If the variable does not exist, an error will be given
setVarInfo<-function(varInfo,newInfo){
  #Find the default setting
  tblVarInfo=getTableVarProperty()
  mandatoryVar=getMandatoryVar()
  defaultTable=getEmpyTable()
  
  #Check if the symbol does not exist in the table
  if(!hasVar(varInfo,newInfo$var,newInfo$version))
    stop(paste0("The given variable is not found: ",var_char))
  
  #Find the index of the variable in the profile table
  var_char=paste0(newInfo$var,"+",newInfo$version)
  var_ind=varInfo$varIndex[[var_char]]
  
  
  varInfo$profile[var_ind,]=newInfo[,mandatoryVar]
  
  #Compare the optional variables in the newInfo with the default setting, if not the same, change it
  optProperty=data.frame()
  optVar=getOptVar()
  for(i in optVar){
    if(newInfo[1,i]!=defaultTable[1,i]){
      optProperty[1,i]=newInfo[,i]
    }
  }
  if(ncol(optProperty)!=0){
    varInfo$optProfile[[var_char]]=optProperty
  }
  return(varInfo)
}

#Add a variable in the table
#If the variable with a give version has exist in the table, an error will be given
addVarInfo<-function(varInfo,newInfo){
  #Find the default setting
  tblVarInfo=getTableVarProperty()
  mandatoryVar=getMandatoryVar()
  defaultTable=getEmpyTable()
  
  #Check if the info already exist
  var_char=paste0(newInfo$var,"+",newInfo$version)
  if(hasVar(varInfo,newInfo$var,newInfo$version))
    stop(paste0("The given variable is already in the table: ",var_char))
  
  varInfo$profile=rbind(varInfo$profile,newInfo[,mandatoryVar])
  varInfo$varIndex[[var_char]]=nrow(varInfo$profile)
  varInfo$varVersion[[newInfo$var]]=newInfo$version
  
  
  #Compare the optional variables in the newInfo with the default setting, if not the same, change it
  optProperty=data.frame()
  optVar=getOptVar()
  for(i in optVar){
    if(newInfo[1,i]!=defaultTable[1,i]){
      optProperty[1,i]=newInfo[,i]
    }
  }
  if(ncol(optProperty)!=0){
    varInfo$optProfile[[var_char]]=optProperty
  }
  return(varInfo)
}



getVarProperty<-function(varInfo,varName,property,version="auto"){
  varName=toCharacter(varName)
  if(version=="auto")
    version=as.numeric(varInfo$varVersion[[varName]])
  
  
  var_char=paste0(varName,"+",version)
  if(!hasVar(varInfo,varName,version))
    stop(paste0("The given variable is not found: ",var_char))
  
  var_ind=varInfo$varIndex[[var_char]]
  
  
  tblVarInfo=getTableVarProperty()
  defaultTable=getEmpyTable()
  optionalInfo=varInfo$optProfile[[var_char]]
  if(is.null(optionalInfo))
    optionalInfo=data.frame()
  
  propertyInfo=data.frame()
  for(prop in property){
    type=tblVarInfo[1,prop]
    if(is.null(type)){
      stop("The property does not exist, please check if you have a typo: ",prop)
    }
    if(type=="mandatory"){
      propertyInfo[1,prop]=varInfo$profile[var_ind,prop]
      next
    }
    if(type=="optional"){
      optInfo=optionalInfo[1,prop]
      if(is.null(optInfo))
        optInfo=defaultTable[1,prop]
      propertyInfo[1,prop]=optInfo
    }
    
  }
  if(ncol(propertyInfo)==1)propertyInfo=propertyInfo[1,1]
  return(propertyInfo)
}


setVarProperty<-function(varInfo,varName,property,value,version="auto"){
  varName=toCharacter(varName)
  if(version=="auto")
    version=as.numeric(varInfo$varVersion[[varName]])
  
  var_char=paste0(varName,"+",version)
  if(!hasVar(varInfo,varName,version))
    stop(paste0("The given variable is not found: ",var_char))
  
  var_ind=varInfo$varIndex[[var_char]]
  
  tblVarInfo=getTableVarProperty()
  optionalInfo=varInfo$optProfile[[var_char]]
  if(is.null(optionalInfo))
    optionalInfo=data.frame()
  for(i in 1:length(property)){
    prop=property[i]
    if(tblVarInfo[1,prop]=="mandatory"){
      varInfo$profile[var_ind,prop]=value[i]
      next
    }
    if(tblVarInfo[1,prop]=="optional"){
      optionalInfo[1,prop]=value[i]
    }
  }
  
  if(ncol(optProperty)!=0){
    varInfo$optProfile[[var_char]]=optionalInfo
  }
  return(varInfo)
}

versionBump<-function(varInfo,varName){
  curInfo_old=getVarInfo(varInfo,varName)
  version=curInfo_old$version
  curInfo_new=curInfo_old
  curInfo_new$version=version+1
  varInfo=setVarInfo(varInfo,curInfo_new)
  varInfo
}

printVarInfo<-function(varInfo){
  info=c()
  for(i in 1:nrow(varInfo$profile)){
    info=rbind(info,getVarInfo(varInfo,varInfo$profile[i,]$var,varInfo$profile[i,]$version))
  }
  info
}

