T_scale="scale"
T_matrix="matrix"

RCcompilerLevel3<-function(compileInfo){
  tmpInd=compileInfo$tmpInd
  parsedExp=compileInfo$code
  bugLog=compileInfo$bugLog
  varInfo=compileInfo$varInfo
  profile=varInfo$profile
  
  tmp_var="gpuMagic_tmp"
  tmp_vat_unitLength=getTypeSize(T_I32)
  tmp_offset=0
  
  gpu_code=c()
  for(i in 1:nrow(profile)){

    curVar=profile[i,]
    if(curVar$require=="Y"){
      curVar$address=curVar$var
      curVar$dataType=T_matrix
    }
    if(curVar$dataType==T_scale){
      CXXtype=getTypeCXXStr(as.numeric(curVar$precisionType))
      if(curVar$compileData=="Y"){
        value=eval(parse(text=curVar$value))
        curCode=paste(CXXtype,curVar$var,"=",value,";",sep = " ")
      }else{
        curCode=paste(CXXtype,curVar$var,";",sep = " ")
      }
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      next
    }
    
    if(curVar$dataType==T_matrix){
      dataType=as.numeric(curVar$precisionType)
      CXXtype=getTypeCXXStr(dataType)
      CXXsize=getTypeSize(dataType)
      size1=evalChar(curVar$size1)
      size2=evalChar(curVar$size2)
      len=size1*size2
      curCode=paste0(CXXtype,"* ",curVar$var,"=","(",CXXtype,"*)(",tmp_var,"+",tmp_offset,");")
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      tmp_offset=tmp_offset+len*CXXsize/tmp_vat_unitLength
      next
    }
  }
  
  varInfo$profile=profile
  
  gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
      
  return(list(tmpInd=tmpInd,code=parsedExp,bugLog=bugLog,varInfo=varInfo,gpu_code=gpu_code,tmp_var=tmp_var))
}


RCTranslation<-function(varInfo,parsedExp){
  gpu_code=c()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(switch(deparse(curExp[[1]]),"="=T,"=="=T,F)){
      curCode=C_call_assign(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
    }
  }
  return(gpu_code)
  
}
#Transfer the expression to the number
evalChar<-function(Exp){
  return(eval(parse(text=Exp)))
}

#Profile the variables
RCcompilerLevel2<-function(compileInfo){
  tmpInd=compileInfo$tmpInd
  parsedExp=compileInfo$code
  bugLog=compileInfo$bugLog
  varInfo=compileInfo$varInfo
  
  
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      #Extract the variable in the left expression
      if(length(leftExp)==1)
        var_char=deparse(leftExp)
      else{
        if(leftExp[[1]]=="[")
          var_char=deparse(leftExp[[2]])
        else
          stop("Unrecognized code",deparse(curExp))
      }
      #Check if the variable is in the variable table, if not, profile the variable
      if(!has.key(var_char,varInfo$varTable)){
        ExpProfile=getExpInfo(varInfo,rightExp)
        ExpProfile$var=var_char
        
        varInfo$profile=rbind(varInfo$profile,ExpProfile)
        varInfo$varTable[[var_char]]=nrow(varInfo$profile)
      }
        
    }
  }
  return(list(tmpInd=tmpInd,code=parsedExp,bugLog=bugLog,varInfo=varInfo))
}
#Get the right expression profile
getExpInfo<-function(varInfo,Exp){
  if(is.numeric(Exp)){
    ExpInfo=call_numeric(varInfo,Exp)
    return(ExpInfo)
  }
  #If the expression is a function call
  if(is.call(Exp)){
    if(Exp[[1]]=="nrow"||Exp[[1]]=="ncol"||Exp[[1]]=="length"){
      ExpInfo=call_size(varInfo,Exp)
      return(ExpInfo)
    }
    #If the expression is creating a matrix
    if(Exp[[1]]=="matrix"){
      ExpInfo=call_matrix(varInfo,Exp)
      return(ExpInfo)
    }
    if(Exp[[1]]=="+"||Exp[[1]]=="-"||Exp[[1]]=="*"||Exp[[1]]=="/"){
      ExpInfo=call_arithmetic(varInfo,Exp)
      return(ExpInfo)
    }
    if(Exp[[1]]=="["){
      ExpInfo=call_subset(varInfo,Exp)
      return(ExpInfo)
    }
    stop("Unsupported function: ",deparse(Exp))
  }
  
  #If not the above case, the expression will be treated as a variable
  ExpInfo=getEmpyTable(1)
  var_data=getVarInfo(varInfo,Exp)
  content=c("dataType","precisionType", "size1","size2","value",
            "compileSize","compileData")
  ExpInfo[,content]=var_data[,content]
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
            "compileData","require")
  tbl=as.data.frame(matrix("NA",ncol = length(tlbName), nrow = rowNum))
  names(tbl)=tlbName
  if(rowNum!=0){
    tbl$precisionType=T_DEFAULT
    tbl$compileSize="N"
    tbl$compileData="N"
    tbl$require="N"
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


#Profile the parameters only
RCcompilerLevel1<-function(compileInfo,parms){
  tmpInd=compileInfo$tmpInd
  parsedExp=compileInfo$code
  bugLog=compileInfo$bugLog
  varInfo=list()
  varInfo$profile=getEmpyTable(0)
  varInfo$requiredVar=c()
  varInfo$varTable=hash()
  
  varName=names(parms)
  for(i in 1:length(parms)){
    if(class(parms[[i]])!="gpuMatrix"){
      curDim=dim(as.matrix(parms[[i]]))
      
    }else{
      curDim=dim(parms[[i]])
    }
    info=getEmpyTable(1)
    info$var=varName[i]
    
    info$precisionType=T_DEFAULT_float
    info$size1=curDim[1]
    info$size2=curDim[2]
    if(curDim[1]==1&&curDim[2]==1){
      info$dataType=T_scale
      info$value=parms[[i]]
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
  return(list(tmpInd=tmpInd,code=parsedExp,bugLog=bugLog,varInfo=varInfo))
}
