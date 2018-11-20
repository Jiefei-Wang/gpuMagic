profile_symbol_left<-function(level,codeMetaInfo,varInfo,curExp){
  leftExp=curExp[[2]]
  rightExp=curExp[[3]]
  leftVar_char=deparse(leftExp)
  
  if(is.call(rightExp)&&(rightExp[[1]]=="gMatrix"||rightExp[[1]]=="gNumber")){
    if(hasVar(varInfo,leftVar_char)){
      tmpMeta=codeMetaInfo$tmpMeta
      tmpMeta=getTmpVar(tmpMeta)
      newName=tmpMeta$varName
      curExp[[2]]=as.symbol(newName)
      renameList=matrix(c(leftVar_char,newName),1)
      
      curInfo=getExpInfo(varInfo,rightExp)
      curInfo$var=leftVar_char
      varInfo=addVarInfo(varInfo,curInfo)
      return(list(Exp=curExp,varInfo=varInfo,renameList=renameList,tmpMeta=tmpMeta))
    }else{
      curInfo=getExpInfo(varInfo,rightExp)
      curInfo$var=leftVar_char
      varInfo=addVarInfo(varInfo,curInfo)
      return(list(Exp=curExp,varInfo=varInfo))
    }
  }
  
  
  
  if(hasVar(varInfo,leftVar_char)){
    leftInfo=getVarInfo(varInfo,leftVar_char)
    
    #If the definition of the left expression cannot be changed, skip it
    if(leftInfo$constDef)
      return(list())
    if(leftInfo$constVal)
      stop("The left expression is a constant, changing the number of the left expression is not allowed:\n:",deparse(curExp))
    
    rightInfo=getExpInfo(varInfo,rightExp)
    checkInfo=checkVarType(leftInfo,rightInfo)
    #Resize function is not working now, it needs some optimization
    if(checkInfo$needReassign){
      if("for" %in% level || "if" %in% level)
        stop("Type conversion inside the for or if body is not allowed, please assign the variable before it:\n:",
             paste0("TraceBack:",paste0(level,collapse = "->"),"\n"),
             deparse(curExp))
      tmpMeta=codeMetaInfo$tmpMeta
      tmpMeta=getTmpVar(tmpMeta)
      newName=tmpMeta$varName
      curExp[[2]]=as.symbol(newName)
      renameList=matrix(c(leftVar_char,newName),1)
      leftInfo=copyVarInfo(rightInfo)
      leftInfo$var=newName
      varInfo=addVarInfo(varInfo,leftInfo)
      return(list(varInfo=varInfo,renameList=renameList,tmpMeta=tmpMeta))
    }
    
    if(checkInfo$needResize){
      leftInfo$value=rightInfo$value
      leftInfo$precisionType=rightInfo$precisionType
      leftInfo$size1=rightInfo$size1
      leftInfo$size2=rightInfo$size2
      leftInfo$compileSize1=rightInfo$compileSize1
      leftInfo$compileSize2=rightInfo$compileSize2
      leftInfo$compileValue=rightInfo$compileValue
      leftInfo$version=leftInfo$version+1
      varInfo=addVarInfo(varInfo,leftInfo)
      #version bump
      versionBump=parse(text=paste0(GPUVar$preservedFuncPrefix,"setVersion(",leftVar_char,",",leftInfo$version,")"))[[1]]
      
      return(list(varInfo=varInfo,extCode=versionBump))
    }
    
    
    if(checkInfo$needRetype||leftInfo$value!=rightInfo$value||leftInfo$compileValue!=rightInfo$compileValue){
      if(checkInfo$needRetype){
        leftInfo$precisionType=rightInfo$precisionType
      }
      leftInfo$value=rightInfo$value
      leftInfo$compileValue=rightInfo$compileValue
      leftInfo$version=leftInfo$version+1
      varInfo=addVarInfo(varInfo,leftInfo)
      #version bump
      versionBump=parse(text=paste0(GPUVar$preservedFuncPrefix,"setVersion(",leftVar_char,",",leftInfo$version,")"))[[1]]
      return(list(varInfo=varInfo,extCode=versionBump))
    }
  }else{
    rightInfo=getExpInfo(varInfo,rightExp)
    leftInfo=copyVarInfo(rightInfo)
    leftInfo$var=leftVar_char
    varInfo=addVarInfo(varInfo,leftInfo)
    return(list(varInfo=varInfo))
  }
  
}


profile_size<-function(varInfo,Exp){
  ExpInfo=getEmpyTable(T_scale)
  ExpInfo$precisionType=GPUVar$default_index_type
  if(Exp[[1]]=="nrow"){
    ExpInfo$value=getVarProperty(varInfo,Exp[[2]],"size1")
    ExpInfo$compileValue=getVarProperty(varInfo,Exp[[2]],"compileSize1")
  }
  if(Exp[[1]]=="ncol"){
    ExpInfo$value=getVarProperty(varInfo,Exp[[2]],"size2")
    ExpInfo$compileValue=getVarProperty(varInfo,Exp[[2]],"compileSize2")
    }
  if(Exp[[1]]=="length"){
    ExpInfo$compileValue=getVarProperty(varInfo,Exp[[2]],"compileSize1")&&getVarProperty(varInfo,Exp[[2]],"compileSize2")
    if(ExpInfo$compileValue){
      Exp$value=Simplify2(paste0(getVarProperty(varInfo,Exp[[2]],"size1"),"*",getVarProperty(varInfo,Exp[[2]],"size2")))
    }
  }
  return(ExpInfo)
  
}



# Exp=parse(text="matrix(10,2)")[[1]]
# Exp=parse(text="matrix(a,2,2)")[[1]]
# Exp=parse(text="matrix(a,2)")[[1]]
# Exp=parse(text="matrix(a,ncol=2)")[[1]]
# Exp=parse(text="matrix(a,2,2)")[[1]]
#matchFunArg(matrix,Exp)


profile_matrix<-function(varInfo,Exp){
  #Get the matrix data and size
  args=matchFunArg(matrix,Exp)
 
  data=args$data
  
  if(is.na(data))
    stop("Unrecognized code: ",deparse(Exp))
  
  #If the element is a numeric value(The currently only available one)
  if(is.numeric(data)){
    #Check if the row and col is the number, if not, find the value of it.
    #If the value cannot be determined, an error will be given
    if(!is.numeric(args$nrow)){
      var_data=getVarInfo(varInfo,args$nrow)
      if(var_data$compileValue==TRUE){
        if(var_data$dataType==T_scale)
          rowNum=var_data$value
        else
          if(var_data$size1==1&&var_data$size2==1)
            rowNum=paste0(var_data$value,"[1]")
          else
            stop("Unsupported code: ",deparse(Exp))
      }else{
        stop("Unsupported code: ",deparse(Exp))
      }
    }else{
      rowNum=deparse(args$nrow)
    }
    if(!is.numeric(args$ncol)){
      var_data=getVarInfo(varInfo,args$ncol)
      if(var_data$compileValue==TRUE){
        if(var_data$dataType==T_scale)
          colNum=var_data$value
        else
          if(var_data$size1==1&&var_data$size2==1)
            colNum=paste0(var_data$value,"[1]")
          else
            stop("Unsupported code: ",deparse(Exp))
      }else{
        stop("Unsupported code: ",deparse(Exp))
      }
    }else{
      colNum=deparse(args$ncol)
    }
    ExpInfo=getEmpyTable()
    ExpInfo$dataType="matrix"
    ExpInfo$precisionType=gpuMagic.option$getDefaultFloat()
    ExpInfo$size1=rowNum
    ExpInfo$size2=colNum
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
    ExpInfo$compileValue=TRUE
    ExpInfo$value=paste0("matrix(",data,",",rowNum,",",colNum,")")
    return(ExpInfo)
  }
  if(!is.numeric(data)){
    stop("Unsupported code: ",ExpRecord)
  }
}





# Exp=parse(text="1+100")[[1]]
#profile_arithmetic(varInfo,quote(ind+1))


profile_arithmetic<-function(varInfo,Exp){
  ExpInfo=getEmpyTable()
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  if(is.numeric(leftExp)){
    leftInfo=getEmpyTable(type=T_scale)
    leftInfo$compileValue=TRUE
    leftInfo$value=deparse(leftExp)
  }else{
    leftInfo=getVarInfo(varInfo,leftExp)
  }
  if(is.numeric(rightExp)){
    rightInfo=getEmpyTable(type=T_scale)
    rightInfo$compileValue=TRUE
    rightInfo$value=deparse(rightExp)
  }else{
    rightInfo=getVarInfo(varInfo,rightExp)
  }
  ExpInfo$precisionType=typeInherit(leftInfo$precisionType,rightInfo$precisionType)
  if(leftInfo$compileSize1&&rightInfo$compileSize1&&leftInfo$compileSize2&&rightInfo$compileSize2){
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
  }else
    stop("Dynamic matrix allocation is not allowed: ",deparse(Exp))
  if(leftInfo$compileValue==TRUE&&rightInfo$compileValue==TRUE){
    ExpInfo$compileValue=TRUE
    ExpInfo$value=Simplify(paste0("(",leftInfo$value,deparse(Exp[[1]]),rightInfo$value,")"))
  }
  if(leftInfo$dataType==T_scale&&rightInfo$dataType==T_scale){
    ExpInfo$dataType=T_scale
    ExpInfo$size1=1
    ExpInfo$size2=1
    ExpInfo$location="local"
  }
  if(leftInfo$dataType==T_scale&&rightInfo$dataType==T_matrix){
    ExpInfo$dataType=T_matrix
    ExpInfo$size1=rightInfo$size1
    ExpInfo$size2=rightInfo$size2
  }
  if(leftInfo$dataType==T_matrix&&rightInfo$dataType==T_scale){
    ExpInfo$dataType=T_matrix
    ExpInfo$size1=leftInfo$size1
    ExpInfo$size2=leftInfo$size2
  }
  if(leftInfo$dataType==T_matrix&&rightInfo$dataType==T_matrix){
    if(leftInfo$size1!=rightInfo$size1||leftInfo$size2!=rightInfo$size2)
      stop("The matrix size does not match: ",deparse(Exp))
    ExpInfo$dataType=T_matrix
    ExpInfo$size1=leftInfo$size1
    ExpInfo$size2=leftInfo$size2
  }
  return(ExpInfo)
}

profile_matrixMult<-function(varInfo,Exp){
  ExpInfo=getEmpyTable(T_matrix)
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  leftInfo=getVarInfo(varInfo,leftExp)
  rightInfo=getVarInfo(varInfo,rightExp)
  if(leftInfo$size2!=rightInfo$size1){
    #warning("Undetermined/Uncomfortable matrix dimension: \n",deparse(Exp),
    #        "\n If the variables are the function arguments, The result may be still valid")
  }
  ExpInfo$size1=leftInfo$size1
  ExpInfo$size2=rightInfo$size2
  if(leftInfo$compileSize1&&rightInfo$compileSize2){
    ExpInfo$compileSize=TRUE
  }
  ExpInfo
}


getSubInfo<-function(varInfo,curInfo,sub_var,i=NA){
  sub=list()
  if(sub_var==""){
    if(is.na(i)){
      sub$compileValue=curInfo$compileSize1&&curInfo$compileSize2
      sub$compileSize=curInfo$compileSize1&&curInfo$compileSize2
      sub$size=Simplify(paste0("(",curInfo$size1,"*",curInfo$size2,")"))
      sub$value=Simplify(paste0("1:(",curInfo$size1,"*",curInfo$size2,")"))
    }else{
      sub$compileValue=curInfo[[paste0("compileSize",i)]]
      sub$compileSize=curInfo[[paste0("compileSize",i)]]
      sub$size=curInfo[[paste0("size",i)]]
      sub$value=Simplify(paste0("1:(",curInfo[[paste0("size",i)]],")"))
    }
    if(sub$size=="1")
      sub$type=T_scale
    else
      sub$type=T_matrix
    
  }else{
    if(is.numeric(sub_var)){
      sub$compileValue=TRUE
      sub$compileSize=TRUE
      sub$value=as.numeric(sub_var)
      sub$size=1
      sub$type=T_scale
    }else{
      subVar=getVarInfo(varInfo,sub_var)
      sub$compileValue=subVar$compileValue
      sub$compileSize=subVar$compileSize1&&subVar$compileSize2
      sub$value=subVar$value
      sub$size=Simplify(paste0("(",subVar$size1,"*",subVar$size2,")"))
      sub$type=ifelse(sub$size=="1",T_scale,T_matrix)
    }
  }
  sub
}

profile_subset<-function(varInfo,Exp){
  curInfo=getVarInfo(varInfo,Exp[[2]])
  sub1=list()
  sub2=list()
  if(length(Exp)==3){
    sub1=getSubInfo(varInfo,curInfo,Exp[[3]])
  }
  
  if(length(Exp)==4){
    sub1=getSubInfo(varInfo,curInfo,Exp[[3]],1)
    sub2=getSubInfo(varInfo,curInfo,Exp[[4]],2)
  }
  
  
  ExpInfo=getEmpyTable(1)
  ExpInfo$dataType=T_matrix
  if(length(Exp)==4){
    if(sub1$type==T_scale&&sub2$type==T_scale){
      ExpInfo$dataType=T_scale
    }
    
    if(ExpInfo$dataType==T_matrix&&sub1$compileValue&&sub2$compileValue&&curInfo$compileValue){
      ExpInfo$value=Simplify(paste0("(",curInfo$value,"[",sub1$value,",",sub2$value,"])"))
      ExpInfo$compileValue=TRUE
    }
    if(sub1$compileSize&&sub2$compileSize){
      ExpInfo$compileSize1=TRUE
      ExpInfo$compileSize2=TRUE
      ExpInfo$size1=sub1$size
      ExpInfo$size2=sub2$size
    }else{
      stop("undetermined size: ",Exp)
    }
  }
  if(length(Exp)==3){
    if(sub1$type==T_scale){
      ExpInfo$dataType=T_scale
    }
    if(ExpInfo$dataType==T_matrix&&sub1$compileValue&&curInfo$compileValue){
      ExpInfo$value=Simplify(paste0("(",curInfo$value,"[",sub1$value,"])"))
      ExpInfo$compileValue=TRUE
    }
    if(sub1$compileSize){
      ExpInfo$compileSize1=TRUE
      ExpInfo$compileSize2=TRUE
      ExpInfo$size1=sub1$size
      ExpInfo$size2=1
    }else{
      stop("undetermined size: ",Exp)
    }
  }

  ExpInfo$precisionType=curInfo$precisionType
  return(ExpInfo)
}

profile_numeric<-function(Exp){
  ExpInfo=getEmpyTable(type=T_scale)
  ExpInfo$value=as.character(Exp)
  ExpInfo$compileValue=TRUE
  return(ExpInfo)
}
profile_symbol<-function(varInfo,Exp){
  var_data=getVarInfo(varInfo,Exp)
  var_data
}

profile_floor<-function(varInfo,Exp){
  ExpInfo=profile_symbol(varInfo,Exp[[2]])
  ExpInfo$precisionType=gpuMagic.option$getDefaultInt()
  return(ExpInfo)
}
profile_ceil<-function(varInfo,Exp){
  ExpInfo=profile_symbol(varInfo,Exp[[2]])
  ExpInfo$precisionType=gpuMagic.option$getDefaultInt()
  return(ExpInfo)
}

profile_return<-function(varInfo,Exp){
  ExpInfo=profile_symbol(varInfo,Exp[[2]])
  ExpInfo$var=GPUVar$gpu_return_variable
  return(ExpInfo)
}

profile_gMatrix<-function(varInfo,Exp){
  args=matchFunArg(gMatrix,Exp)
  args$nrow=parse(text=args$nrow)[[1]]
  args$ncol=parse(text=args$ncol)[[1]]
  if(!is.numeric(args$nrow)){
    varData=getVarInfo(varInfo,args$nrow)
    if(varData$compileValue==FALSE)
      stop("Unable to determine the size of the matrix:\n",deparse(Exp))
    if(varData$size1!=1||varData$size2!=1)
      stop("Illigel row argument:\n",deparse(Exp))
    args$nrow=Simplify(paste0(varData$value,"[1]"))
  }
  if(!is.numeric(args$ncol)){
    varData=getVarInfo(varInfo,args$ncol)
    if(varData$compileValue==FALSE)
      stop("Unable to determine the size of the matrix:\n",deparse(Exp))
    if(varData$size1!=1||varData$size2!=1)
      stop("Illigel column argument:\n",deparse(Exp))
    args$ncol=Simplify(paste0(varData$value,"[1]"))
  }
  if(args$precision=="gpuMagic.option$getDefaultFloat"){
    args$precision=gpuMagic.option$getDefaultFloat()
  }
  ExpInfo=getEmpyTable(1)
  ExpInfo$dataType=T_matrix
  ExpInfo$precisionType=args$precision
  ExpInfo$size1=args$nrow
  ExpInfo$size2=args$ncol
  ExpInfo$constDef=args$constDef
  ExpInfo$compileSize1=TRUE
  ExpInfo$compileSize2=TRUE
  ExpInfo$location=args$location
  ExpInfo$shared=args$shared
  return(ExpInfo)
}


profile_gNumber<-function(varInfo,Exp){
  args=matchFunArg(gNumber,Exp)
  if(args$precision=="gpuMagic.option$getDefaultFloat"){
    args$precision=gpuMagic.option$getDefaultFloat()
  }
  ExpInfo=getEmpyTable(1,type=T_scale)
  ExpInfo$precisionType=args$precision
  ExpInfo$constDef=args$constDef
  return(ExpInfo)
}
#This function works only when the code is not transpose the matrix, but create a new matrix
#eg: B=t(A)
profile_transpose<-function(varInfo,Exp){
  ExpInfo=getVarInfo(varInfo,Exp[[2]])
  size1=ExpInfo$size2
  size2=ExpInfo$size1
  ExpInfo$size1=size1
  ExpInfo$size2=size2
  compileSize1=ExpInfo$compileSize2
  compileSize2=ExpInfo$compileSize1
  ExpInfo$size1=compileSize1
  ExpInfo$size2=compileSize2
  
  ExpInfo
}


profile_resize<-function(varInfo,Exp){
  args=matchFunArg(resize,Exp)
  if(args$data==""||args$nrow==""||args$ncol=="")
    stop("The arguments are incomplete:\n",deparse(Exp))
  args$nrow=parse(text=args$nrow)[[1]]
  args$ncol=parse(text=args$ncol)[[1]]
  if(!is.numeric(args$nrow)){
    varData=getVarInfo(varInfo,args$nrow)
    if(varData$compileValue==FALSE)
      stop("Unable to determine the size of the matrix:\n",deparse(Exp))
    if(varData$size1!=1||varData$size2!=1)
      stop("Illigel row argument:\n",deparse(Exp))
    args$nrow=Simplify(paste0(varData$value,"[1]"))
  }
  if(!is.numeric(args$ncol)){
    varData=getVarInfo(varInfo,args$ncol)
    if(varData$compileValue==FALSE)
      stop("Unable to determine the size of the matrix:\n",deparse(Exp))
    if(varData$size1!=1||varData$size2!=1)
      stop("Illigel column argument:\n",deparse(Exp))
    args$ncol=Simplify(paste0(varData$value,"[1]"))
  }
  
  ExpInfo=getVarInfo(varInfo,Exp[[1]])
  ExpInfo$size1=args$nrow
  ExpInfo$size2=args$ncol
  ExpInfo$compileSize=TRUE
  return(ExpInfo)
}








