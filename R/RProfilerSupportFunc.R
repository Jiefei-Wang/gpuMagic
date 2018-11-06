
#===========================profiler 1========================

#Profile a parameter and give the profile table back
profileVar<-function(parms,staticParms){
  varInfo=list()
  varInfo$profile=getEmpyTable(0)
  varInfo$requiredVar=c()
  varInfo$varTable=hash()
  varInfo$varVersion=hash()
  varInfo$profileTblName="profile"
  if(length(parms)==0) return(varInfo)
  varName=names(parms)
  for(i in 1:length(parms)){
    if(class(parms[[i]])=="gpuMatrix"){
      curPrecision=getTypeNum(.type(parms[[i]]))
      curDim=dim(parms[[i]])
    }else{
      curPrecision=gpuMagic.option$getDefaultFloat()
      curDim=dim(as.matrix(parms[[i]]))
    }
    info=getEmpyTable(1)
    info$var=varName[i]
    
    info$precisionType=curPrecision
    
    if(varName[i] %in% names(staticParms)){
      info$constant=TRUE
      info$size1=nrow(as.matrix(parms[[i]]))
      info$size2=ncol(as.matrix(parms[[i]]))
      info$value=paste0("c(",paste0(parms[[i]],collapse = ","),")")
      info$compileSize=TRUE
      info$compileData=TRUE
      info$require=FALSE
      info$initialization=FALSE
    }else{
      info$size1=paste0("(",varInfo$profileTblName,"[",i,",]","$size1)")
      info$size2=paste0("(",varInfo$profileTblName,"[",i,",]","$size2)")
      
      if(curDim[1]==1&&curDim[2]==1){
        info$dataType=T_scale
        info$size1=1
        info$size2=1
        info$value=paste0("(",varInfo$profileTblName,"[",i,",]","$value)")
        info$compileData=TRUE
      }else{
        info$dataType=T_matrix
      }
      info$compileSize=TRUE
      info$require=TRUE
    }
    varInfo=addVarInfo(varInfo,info)
    varInfo$requiredVar=c(varInfo$requiredVar,info$var)
  }
  varInfo
}
renameLoopVar<-function(parsedExp,ind=0){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(!is.call(curExp))
      next
    if(curExp[[1]]=="for"){
      #Force substitution of the index variable
      new_index=paste0(GPUVar$gpu_loop_ind,"_",ind)
      ind=ind+1
      old_Index=deparse(curExp[[2]])
      loopBody=renameVarInCode(curExp[[4]],1,old_Index,new_index)
      res=renameLoopVar(loopBody,ind)
      loopBody=res$parsedExp
      ind=res$ind
      curExp[[2]]=as.symbol(new_index)
      curExp[[4]]=loopBody
      parsedExp[[i]]=curExp
    }
    if(curExp[[1]]=="if"){
      res=renameLoopVar(curExp[[3]],ind)
      codeBody=res$parsedExp
      ind=res$ind
      curExp[[3]]=codeBody
      parsedExp[[i]]=curExp
      if(length(curExp)==4){
        res=renameLoopVar(curExp[[4]],ind)
        codeBody=res$parsedExp
        ind=res$ind
        curExp[[4]]=codeBody
        parsedExp[[i]]=curExp
      }
    }
  }
  return(list(parsedExp=parsedExp,ind=ind))
}

profileLoopVar<-function(varInfo,parsedExp){
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(!is.call(curExp))
      next
    if(curExp[[1]]=="for"){
      var_char=deparse(curExp[[2]])
      ExpProfile=getEmpyTable(1,type = T_scale)
      ExpProfile$var=var_char
      ExpProfile$initialization=FALSE
      varInfo=addVarInfo(varInfo,ExpProfile)
      loopBody=curExp[[4]]
      varInfo=profileLoopVar(varInfo,loopBody)
    }
    if(curExp[[1]]=="if"){
      varInfo=profileLoopVar(varInfo,curExp[[3]])
      if(length(curExp)==4){
        varInfo=profileLoopVar(varInfo,curExp[[4]])
      }
    }
  }
  return(varInfo)
}

#==================================Profiler 2==========================

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
getVarInfo<-function(varInfo,varName,version="auto"){
  if(is.character(varName))
    var_char=varName
  else
    var_char=deparse(varName)
  
  #Check if the symbol does not exist in the table
  if(!has.key(var_char,varInfo$varVersion))
    stop(paste0("The given variable is not found: ",var_char))
  if(version=="auto")
    version=as.numeric(varInfo$varVersion[[var_char]])
  var_char=paste0(var_char,"+",version)
  var_ind=varInfo$varTable[[var_char]]
  var_data=varInfo$profile[var_ind,,drop=F]
  var_data
}
setVarInfo<-function(varInfo,newInfo){
  var_char=paste0(newInfo$var,"+",newInfo$version)
  #Check if the symbol does not exist in the table
  if(!has.key(var_char,varInfo$varTable))
    stop(paste0("The given variable is not found: ",var_char))
  var_ind=varInfo$varTable[[var_char]]
  varInfo$profile[var_ind,]=newInfo
  varInfo
}
addVarInfo<-function(varInfo,newInfo){
  version=as.numeric(newInfo$version)
  var_char=paste0(newInfo$var,"+",version)
  if(has.key(var_char,varInfo$varTable)){
    version=version+1
    var_char=paste0(newInfo$var,"+",version)
  }
  varInfo$profile=rbind(varInfo$profile,newInfo)
  varInfo$varTable[[var_char]]=nrow(varInfo$profile)
  varInfo$varVersion[[newInfo$var]]=version
  varInfo
}


checkVarType<-function(leftInfo,rightInfo){
  if(leftInfo$constant)
    stop("The static variable cannot be changed:\n",leftInfo$var)
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
            "compileData","require","initialization","global_share","constant","location","version")
  boolVar=c("compileSize","compileData","require","initialization","global_share","constant")
  tbl=as.data.frame(matrix("NA",ncol = length(tlbName), nrow = rowNum))
  names(tbl)=tlbName
  if(rowNum!=0){
    tbl$precisionType=gpuMagic.option$getDefaultFloat()
    tbl$compileSize=FALSE
    tbl$compileData=FALSE
    tbl$require=FALSE
    tbl$initialization=TRUE
    tbl$global_share=FALSE
    tbl$constant=FALSE
    tbl$version="1"
    tbl$location="global"
    if(type==T_scale){
      tbl$dataType=T_scale
      tbl$compileSize=TRUE
      tbl$size1=1
      tbl$size2=1
      tbl$location="private"
    }
    if(type==T_matrix){
      tbl$dataType=T_matrix
    }
  }
  for(i in colnames(tbl)){
    tbl[,i]=as.character(tbl[,i])
  }
  
  for(i in boolVar){
    tbl[,i]=as.logical(tbl[,i])
  }
  tbl
}
#Determine which type can preserve the information 
#of the information in type1 and type2
typeInherit<-function(type1,type2){
  group_float=c("half","float","double")
  group_int=c("char","int","long","uint","ulong")
  
  target_size=max(getTypeSize(type1),getTypeSize(type2))
  if(type1 %in% group_float||type2%in% group_float){
    for(i in 1:length(group_float)){
      if(target_size==getTypeSize(group_float[i]))
         return(group_float[i])
    }
  }else{
    if(type1 %in% group_int||type2%in% group_int){
      for(i in 1:length(group_int)){
        if(target_size==getTypeSize(group_int[i]))
          return(group_int[i])
      }
    }else{
      warning("Unsupported type inherit: ",type1,"+",type2)
      return(gpuMagic.option$getDefaultFloat())
    }
  }
}

is.preservedFunc<-function(func){
  func=as.character(func)
  length(grep(GPUVar$preservedFuncPrefix,func,fixed = T))!=0
}
