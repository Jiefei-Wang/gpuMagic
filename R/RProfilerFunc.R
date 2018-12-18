profile_symbol_left<-function(level,codeMetaInfo,varInfo,curExp){
  leftExp=curExp[[2]]
  rightExp=curExp[[3]]
  leftVar_char=deparse(leftExp)
  
  #Determine if the right expression explicitly define a variable
  varDefine=FALSE
  if(is.call(rightExp)){
    varDefine=deparse(rightExp[[1]])%in% .profileVarDefine
  }
  
  rightInfo=getExpInfo(varInfo,rightExp)
  
  if(hasVar(varInfo,leftVar_char)){
    leftInfo=getVarInfo(varInfo,leftVar_char)
    
    #If the definition of the left expression cannot be changed, skip it
    if(leftInfo$constDef)
      return(list())
    if(leftInfo$lazyRef)
      return(list())
    if(leftInfo$constVal)
      stop("The left expression is a constant, changing the number of the left expression is not allowed:\n:",deparse(curExp))
    if(!rightInfo$compileSize1||!rightInfo$compileSize2)
      stop("Unable to determine the matrix size: ",deparse(curExp))
    
    
    checkInfo=checkVarType(leftInfo,rightInfo)
    #If the variable need to be redefined, or user explicitly define the variable
    if(checkInfo$needReassign||varDefine){
      if("for" %in% level || "if" %in% level)
        stop("Type conversion inside the for or if body is not allowed, please assign the variable before it:\n:",
             paste0("TraceBack:",paste0(level,collapse = "->"),"\n"),
             deparse(curExp))
      newName=GPUVar$getTmpVar()
      curExp[[2]]=as.symbol(newName)
      renameList=matrix(c(leftVar_char,newName),1)
      if(varDefine){
        leftInfo=copyVarInfo(rightInfo,fullCopy=TRUE)
      }else{
        leftInfo=copyVarInfo(rightInfo)
      }
      leftInfo$var=newName
      varInfo=addVarInfo(varInfo,leftInfo)
      return(list(varInfo=varInfo,renameList=renameList,Exp=curExp))
    }
    
    if(checkInfo$valueUpdate){
      if(checkInfo$needRetype){
        leftInfo$precisionType=rightInfo$precisionType
      }
      leftInfo$value=rightInfo$value
      leftInfo$compileValue=rightInfo$compileValue
      leftInfo$version=leftInfo$version+1
      varInfo=addVarInfo(varInfo,leftInfo)
      #version bump
      versionBump=getVersionBumpCode(leftVar_char,leftInfo$version)
      return(list(varInfo=varInfo,extCode=versionBump))
    }
  }else{
    if(!rightInfo$compileSize1||!rightInfo$compileSize2)
      stop("Unable to determine the matrix size: ",deparse(curExp))
    if(varDefine){
      leftInfo=copyVarInfo(rightInfo,fullCopy=TRUE)
    }else{
      leftInfo=copyVarInfo(rightInfo)
    }
    leftInfo$var=leftVar_char
    leftInfo$version=1
    varInfo=addVarInfo(varInfo,leftInfo)
    return(list(varInfo=varInfo))
  }
  
}


profile_size<-function(varInfo,Exp){
  if(Exp[[1]]=="nrow"){
    ExpInfo=getEmpyTable(T_scale)
    ExpInfo$precisionType=GPUVar$default_index_type
    ExpInfo$value=getVarProperty(varInfo,Exp[[2]],"size1")
    ExpInfo$compileValue=getVarProperty(varInfo,Exp[[2]],"compileSize1")
  }
  if(Exp[[1]]=="ncol"){
    ExpInfo=getEmpyTable(T_scale)
    ExpInfo$precisionType=GPUVar$default_index_type
    ExpInfo$value=getVarProperty(varInfo,Exp[[2]],"size2")
    ExpInfo$compileValue=getVarProperty(varInfo,Exp[[2]],"compileSize2")
    }
  if(Exp[[1]]=="length"){
    ExpInfo=getEmpyTable(T_matrix)
    ExpInfo$precisionType=GPUVar$default_index_type
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
    ExpInfo$compileValue=FALSE
    #ExpInfo$value=paste0("matrix(",data,",",rowNum,",",colNum,")")
    return(ExpInfo)
  }
  if(!is.numeric(data)){
    stop("Unsupported code: ",ExpRecord)
  }
}





# Exp=parse(text="1+100")[[1]]
#profile_arithmetic(varInfo,quote(ind+1))


profile_arithmetic<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  
  leftInfo=getExpInfo(varInfo,leftExp)
  rightInfo=getExpInfo(varInfo,rightExp)
  
  
  if(leftInfo$dataType==T_scale&&rightInfo$dataType==T_scale){
    ExpInfo=getEmpyTable(type=T_scale)
  }else{
    ExpInfo=getEmpyTable(type=T_matrix)
  }
  ExpInfo$precisionType=typeInherit(leftInfo$precisionType,rightInfo$precisionType)
  
  if(leftInfo$compileSize1&&rightInfo$compileSize1&&leftInfo$compileSize2&&rightInfo$compileSize2){
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
  }
    
  if(leftInfo$compileValue==TRUE&&rightInfo$compileValue==TRUE){
    ExpInfo$compileValue=TRUE
    ExpInfo$value=Simplify(paste0("(",leftInfo$value,deparse(Exp[[1]]),rightInfo$value,")"))
  }
  
  ExpInfo$size1=Simplify(paste0("max(",leftInfo$size1,",",rightInfo$size1,")"))
  ExpInfo$size2=Simplify(paste0("max(",leftInfo$size2,",",rightInfo$size2,")"))
  
  
  addErrorCheck(varInfo,level="warning",code=deparse(Exp),
                check=paste0("(",leftInfo$size1,"!=",rightInfo$size1,"||",
                             leftInfo$size2,"!=",rightInfo$size2,")",
                             "&&(",leftInfo$size1,"!=1||",leftInfo$size2,"!=1)&&(",
                             rightInfo$size1,"!=1||",rightInfo$size2,"!=1)"),
                msg="Possibly uncomfortable matrix dimension has found")
  
  return(ExpInfo)
}



#%*%
profile_matrixMult<-function(varInfo,Exp){
  ExpInfo=getEmpyTable(T_matrix)
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  leftInfo=getVarInfo(varInfo,leftExp)
  rightInfo=getVarInfo(varInfo,rightExp)
  
  ExpInfo$size1=leftInfo$size1
  ExpInfo$size2=rightInfo$size2
  if(leftInfo$compileSize1&&rightInfo$compileSize2){
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
  }
  
  addErrorCheck(varInfo,level="error",code=deparse(Exp),
                check=paste0(leftInfo$size2,"!=",rightInfo$size1),
                msg="Uncomfortable matrix dimension has found")
  
  ExpInfo
}

#i: indicate the subset index number
#NA: one value subset
#1: first index
#2: second index
getSubInfo<-function(varInfo,curInfo,sub_var,i=NA){
  sub=list()
  
  if(sub_var==""){
    #one index sub
    if(is.na(i)){
      sub$compileValue=curInfo$compileSize1&&curInfo$compileSize2
      sub$compileSize=curInfo$compileSize1&&curInfo$compileSize2
      sub$size=Simplify(paste0("(",curInfo$size1,"*",curInfo$size2,")"))
      sub$value=Simplify(paste0("1:(",curInfo$size1,"*",curInfo$size2,")"))
    }else{
      #two index sub
      sub$compileValue=curInfo[[paste0("compileSize",i)]]
      sub$compileSize=curInfo[[paste0("compileSize",i)]]
      sub$size=curInfo[[paste0("size",i)]]
      sub$value=paste0("(",Simplify(paste0("1:(",curInfo[[paste0("size",i)]],")")),")")
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
      subVarInfo=getVarInfo(varInfo,sub_var)
      sub$compileValue=subVarInfo$compileValue
      sub$compileSize=subVarInfo$compileSize1&&subVarInfo$compileSize2
      sub$value=subVarInfo$value
      sub$size=paste0("(",Simplify(paste0("(",subVarInfo$size1,"*",subVarInfo$size2,")")),")")
      sub$type=ifelse(sub$size=="1",T_scale,T_matrix)
    }
  }
  sub
}

#Exp=quote(A[,ind,drop=T])
profile_subset<-function(varInfo,Exp){
  curInfo=getVarInfo(varInfo,Exp[[2]])
  args=matchBracketFunc(Exp)
  
  sub1=list()
  sub2=list()
  
  #Determine the one sub or two sub
  if(is.null(args$j)){
    sub1=getSubInfo(varInfo,curInfo,args$i)
  }else{
    sub1=getSubInfo(varInfo,curInfo,args$i,1)
    sub2=getSubInfo(varInfo,curInfo,args$j,2)
  }
  
  
  ExpInfo=getEmpyTable(1)
  if(!is.null(args$j)){
    
    if(sub1$compileValue&&sub2$compileValue&&curInfo$compileValue){
      ExpInfo$value=paste0("(",Simplify(paste0(curInfo$value,"[",sub1$value,",",sub2$value,"]")),")")
      ExpInfo$compileValue=TRUE
    }
    if(sub1$compileSize&&sub2$compileSize){
      ExpInfo$compileSize1=TRUE
      ExpInfo$compileSize2=TRUE
      ExpInfo$size1=sub1$size
      ExpInfo$size2=sub2$size
    }
  }
  
  if(is.null(args$j)){
    if(sub1$compileValue&&curInfo$compileValue){
      ExpInfo$value=paste0("(",Simplify(paste0(curInfo$value,"[",sub1$value,"]")),")")
      ExpInfo$compileValue=TRUE
    }
    if(sub1$compileSize){
      ExpInfo$compileSize1=TRUE
      ExpInfo$compileSize2=TRUE
      ExpInfo$size1=sub1$size
      ExpInfo$size2=1
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

#Exp=quote(gMatrix(1,10))
profile_gMatrix<-function(varInfo,Exp){
  args=matchFunArg(gMatrix,Exp)
  args$nrow=parse(text=args$nrow)[[1]]
  args$ncol=parse(text=args$ncol)[[1]]
  if(!isNumeric(args$nrow)){
    varData=getVarInfo(varInfo,args$nrow)
    if(varData$compileValue==FALSE)
      stop("Unable to determine the size of the matrix:\n",deparse(Exp))
    if(varData$size1!=1||varData$size2!=1)
      stop("Illigel row argument:\n",deparse(Exp))
    args$nrow=Simplify(paste0(varData$value,"[1]"))
  }
  if(!isNumeric(args$ncol)){
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

#Exp=quote(subRef(A,tmp))
profile_subRef<-function(varInfo,Exp){
  args=matchFunArg(subRef,Exp)
  if(getVarProperty(varInfo,args$variable,"dataType")!=T_matrix){
    stop("Only matrix is allow to create a reference: ",deparse(Exp))
  }
  if(length(Exp)==3){
    code_char=paste0(args$variable,"[",args$i,"]")
  }else{
    code_char=paste0(args$variable,"[",args$i,",",args$j,"]")
  }
  code=parse(text=code_char)[[1]]
  refInfo=profile_subset(varInfo,code)
  refInfo$initialization=FALSE
  refInfo$lazyRef=TRUE
  refInfo$ref=code_char
  refInfo
}



#This function works only when the code is not transpose the matrix, but create a new matrix
#eg: B=t(A)
profile_transpose<-function(varInfo,Exp){
  stop("The matrix transpose does not work now")
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













