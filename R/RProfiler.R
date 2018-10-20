
#Profile the parameters only
RProfilerLevel1<-function(level3Exp){
  tmpInd=level3Exp$tmpInd
  parsedExp=level3Exp$Exp
  
  #The loop variable needs some special treatment, I rename it so that it would not be confused with the total looped variable
  workerData=names(level3Exp$parms)[1]
  tmp_parms=c(level3Exp$parms)
  names(tmp_parms)[1]=GPUVar$gpu_worker_data
  
  varInfo=profileVar(tmp_parms)
  loopVar=getEmpyTable(1,type=T_scale)
  loopVar$var=workerData
  varInfo$profile=rbind(varInfo$profile,loopVar)
  varInfo$varTable[[workerData]]=nrow(varInfo$profile)
  
  profileMeta1=level3Exp
  profileMeta1$tmpInd=tmpInd
  profileMeta1$Exp=parsedExp
  profileMeta1$varInfo=varInfo
  profileMeta1$workerData=workerData
  
  
  return(profileMeta1)
}

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



#Profile the variables in the code
RProfilerLevel2<-function(profileMeta1){
  tmpInd=profileMeta1$tmpInd
  parsedExp=profileMeta1$Exp
  varInfo=profileMeta1$varInfo
  
  
  for(i in 1:length(parsedExp)){
    #if(i==3)stop()
    curExp=parsedExp[[i]]
    #message(curExp)
    if(curExp=="{"){
      next
    }
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      #Extract the variable in the left expression
      if(is.call(leftExp)){
        if(leftExp[[1]]!="[")
          stop("Unrecognized code",deparse(curExp))
        
      }else{
        var_char=deparse(leftExp)
        #Check if the variable is in the variable table, if not, profile the variable
        if(!has.key(var_char,varInfo$varTable)){
          ExpProfile=getExpInfo(varInfo,rightExp)
          ExpProfile$var=var_char
          
          varInfo$profile=rbind(varInfo$profile,ExpProfile)
          varInfo$varTable[[var_char]]=nrow(varInfo$profile)
        }
      }
      next
    }
    
    if(curExp[[1]]=="return"){
      ExpProfile=profile_return(varInfo,curExp)
      varInfo$returnInfo=ExpProfile
      next
    }
    #For loop
    if(curExp[[1]]=="for"){
      var_char=deparse(curExp[[2]])
      ExpProfile=getEmpyTable(1,type = T_scale)
      ExpProfile$var=var_char
      ExpProfile$initialization="N"
      varInfo$profile=rbind(varInfo$profile,ExpProfile)
      varInfo$varTable[[var_char]]=nrow(varInfo$profile)
      res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[4]],varInfo=varInfo))
      tmpInd=res$tmpInd
      varInfo=res$varInfo
      next
    }
    if(curExp[[1]]=="if"){
      res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[3]],varInfo=varInfo))
      tmpInd=res$tmpInd
      varInfo=res$varInfo
      next
    }
  }
  
  
  profileMeta2=profileMeta1
  profileMeta2$tmpInd=tmpInd
  profileMeta2$Exp=parsedExp
  profileMeta2$varInfo=varInfo
  
  
  return(profileMeta2)
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
#Get an empty profile table
getEmpyTable<-function(rowNum=0,type=""){
  tlbName=c("var","address","dataType","precisionType", "size1","size2","value","compileSize",
            "compileData","require","initialization")
  tbl=as.data.frame(matrix("NA",ncol = length(tlbName), nrow = rowNum))
  names(tbl)=tlbName
  if(rowNum!=0){
    tbl$precisionType=T_DEFAULT
    tbl$compileSize="N"
    tbl$compileData="N"
    tbl$require="N"
    tbl$initialization="Y"
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















