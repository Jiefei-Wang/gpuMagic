
#===========================profiler 1========================
#Profile a parameter and give the profile table back
profileVar<-function(parms){
  varInfo=list()
  varInfo$profile=getEmpyTable(0)
  varInfo$requiredVar=c()
  varInfo$varTable=hash()
  varInfo$profileTblName="profile"
  if(length(parms)==0) return(varInfo)
  varName=names(parms)
  for(i in 1:length(parms)){
    if(class(parms[[i]])=="gpuMatrix"){
      curPrecision=getTypeNum(.type(parms[[i]]))
      curDim=dim(parms[[i]])
    }else{
      curPrecision=T_DEFAULT_float
      curDim=dim(as.matrix(parms[[i]]))
    }
    info=getEmpyTable(1)
    info$var=varName[i]
    
    info$precisionType=curPrecision
    info$size1=paste0("(",varInfo$profileTblName,"[",i,",]","$size1)")
    info$size2=paste0("(",varInfo$profileTblName,"[",i,",]","$size2)")
    if(curDim[1]==1&&curDim[2]==1){
      info$dataType=T_scale
      info$size1=1
      info$size2=1
      info$value=paste0("(",varInfo$profileTblName,"[",i,",]","$value)")
      info$compileData="Y"
    }else{
      info$dataType=T_matrix
    }
    info$compileSize="Y"
    info$require="Y"
    varInfo$profile=rbind(varInfo$profile,info)
    varInfo$requiredVar=c(varInfo$requiredVar,info$var)
    varInfo$varTable[[info$var]]=nrow(varInfo$profile)
  }
  varInfo
}


#==================================Profiler 2==========================

profileLoopVar<-function(varInfo,parsedExp){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(is.call(curExp)&&curExp[[1]]=="for"){
      var_char=deparse(curExp[[2]])
      ExpProfile=getEmpyTable(1,type = T_scale)
      ExpProfile$var=var_char
      ExpProfile$initialization="N"
      varInfo$profile=rbind(varInfo$profile,ExpProfile)
      varInfo$varTable[[var_char]]=nrow(varInfo$profile)
      loopBody=curExp[[4]]
      varInfo=profileLoopVar(varInfo,loopBody)
    }
  }
  return(varInfo)
}
#Find the function parameters
matchFunArg<-function(fun,Exp){
  funArg=lapply(formals(fun),as.character)
  ExpArg=standardise_call(Exp)
  if(length(ExpArg)>1){
    argName=names(ExpArg)
    for(i in 2:length(ExpArg)){
      funArg[[argName[i]]]=deparse(ExpArg[[i]])
    }
  }
  return(funArg)
}
#Get the right expression profile
getExpInfo<-function(varInfo,Exp){
  ExpInfo=NULL
  if(is.numeric(Exp)){
    ExpInfo=profile_numeric(Exp)
    return(ExpInfo)
  }
  #If the expression is a function call
  if(is.call(Exp)){
    func=deparse(Exp[[1]])
    if(!is.null(.profileFuncs[[func]])){
      ExpInfo=.profileFuncs[[func]](varInfo,Exp)
      return(ExpInfo)
    }
    stop("Unsupported function: ",deparse(Exp))
  }
  
  #If not the above case, the expression will be treated as a variable
  if(is.symbol(Exp)){
    ExpInfo=profile_symbol(varInfo,Exp)
    return(ExpInfo)
  }
  if(is.null(ExpInfo))
    stop("Unknow code: ",deparse(Exp))
  
  return(ExpInfo)
}
#Get the variable profile
getVarInfo<-function(varInfo,target){
  if(is.character(target))
    var_char=target
  else
    var_char=deparse(target)
  #Check if the symbol does not exist in the table
  if(!has.key(var_char,varInfo$varTable))
    stop(paste0("The given variable is not found: ",var_char))
  var_ind=varInfo$varTable[[var_char]]
  var_data=varInfo$profile[var_ind,,drop=F]
  var_data
}
setVarInfo<-function(varInfo,newInfo){
  var_char=newInfo$var
  #Check if the symbol does not exist in the table
  if(!has.key(var_char,varInfo$varTable))
    stop(paste0("The given variable is not found: ",var_char))
  var_ind=varInfo$varTable[[var_char]]
  varInfo$profile[var_ind,]=newInfo
}
addVarInfo<-function(varInfo,newInfo){
  var_char=newInfo$var
  varInfo$profile=rbind(varInfo$profile,newInfo)
  varInfo$varTable[[var_char]]=nrow(varInfo$profile)
  varInfo
}


checkVarType<-function(leftInfo,rightInfo){
  needReassign=FALSE
  needResize=FALSE
  needRetype=FALSE
  if(typeInherit(leftInfo$precisionType,rightInfo$precisionType)!=leftInfo$precisionType)
    needRetype=TRUE
  if(leftInfo$dataType!=rightInfo$dataType)
    return(list(needRetype=needRetype,needReassign=TRUE))
  if(leftInfo$size1!=rightInfo$size1||rightInfo$size2!=rightInfo$size2){
    len1=paste0(leftInfo$size1,"*",leftInfo$size2)
    len2=paste0(rightInfo$size1,"*",rightInfo$size2)
    if(Simplify(len1)!=Simplify(len2)){
      return(list(needRetype=needRetype,needReassign=TRUE))
    }else{
      if(is.numeric(Simplify(len1)))
        return(list(needRetype=needRetype,needReassign=FALSE,needResize=TRUE,size1=rightInfo$size1,size2=rightInfo$size2))
    }
  }
  return(list(needRetype=needRetype,needReassign=FALSE,needResize=FALSE))
}

#Get an empty profile table
getEmpyTable<-function(rowNum=0,type=""){
  tlbName=c("var","address","dataType","precisionType", "size1","size2","value","compileSize",
            "compileData","require","initialization","p_static","t_static","location")
  tbl=as.data.frame(matrix("NA",ncol = length(tlbName), nrow = rowNum))
  names(tbl)=tlbName
  if(rowNum!=0){
    tbl$precisionType=T_DEFAULT_float
    tbl$compileSize="N"
    tbl$compileData="N"
    tbl$require="N"
    tbl$initialization="Y"
    tbl$p_static="N"
    tbl$t_static="N"
    tbl$location="global"
    if(type==T_scale){
      tbl$dataType=T_scale
      tbl$compileSize="Y"
      tbl$size1=1
      tbl$size2=1
    }
    if(type==T_matrix){
      tbl$dataType=T_matrix
    }
  }
  for(i in 1:ncol(tbl)){
    tbl[,i]=as.character(tbl[,i])
  }
  tbl
}
#Determine which type can preserve the information 
#of the information in type1 and type2
typeInherit<-function(type1,type2){
  if(type1==T_F64||type2==T_F64)
    return(as.character(T_F64))
  if((type1==T_F32||type2==T_F32)&&(type1==T_I64||type2==T_I64))
    return(as.character(T_F64))
  if(type1==T_I64||type2==T_I64)
    return(as.character(T_I64))
  if(type1==T_F32||type2==T_F32)
    return(as.character(T_F32))
  if(type1==T_I32||type2==T_I32)
    return(as.character(T_I32))
}

