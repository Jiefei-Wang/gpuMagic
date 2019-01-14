#curExp=quote({B=A[1]})[[2]]
profiler_assignment_dispatch<-function(level,varInfo,curExp){
  leftVar=curExp[[2]]
  
  if(hasVar(varInfo,deparse(leftVar))){
    return(profiler_assignment_exitingVar(level,varInfo,curExp))
  }else{
    return(profiler_assignment_newVar(level,varInfo,curExp))
  }
}

#If the left variable exists
profiler_assignment_exitingVar<-function(level,varInfo,curExp){
  extCode=NULL
  leftVar=curExp[[2]]
  rightExp=curExp[[3]]
  
  #Determine if the right expression explicitly or implicitly defines a variable
  defineType="implicit"
  if(is.call(rightExp)&&deparse(rightExp[[1]])%in% .profileExplicitDefine){
    defineType="explicit"
  }
  
  rightInfo=getExpInfo(varInfo,rightExp,errorCheck = T)
  leftInfo=getVarInfo(varInfo,leftVar)
  
  #if the right expression is the sequence function,create a new variable for it
  if(rightInfo$isSeq&&is.call(rightExp)){
    newVar=GPUVar$getTmpVar()
    newExp=parse(text=paste0(newVar,"=",deparse(rightExp)))[[1]]
    newRes=profiler_assignment_newVar(level,varInfo,newExp)
    
    varInfo=newRes[["varInfo"]]
    extCode=newExp
    
    rightExp=as.symbol(newVar)
    rightInfo=getExpInfo(varInfo,rightExp)
    curExp[[3]]=rightExp
  }
  #If the left expression is a lazy reference, then do not perform the profiling
  if(leftInfo$constDef){
    return(list(extCode=extCode,Exp=curExp))
  }
  if(leftInfo$isSeq){
    
    stop("The left variable is in a compressed format and cannot be changed: ",deparse(curExp))
  }
  if(leftInfo$constVal){
    stop("The left variable is a constant and cannot be changed: ",deparse(curExp))
  }
  
  
  action=0
  warningLevel=0
  propertyNames=colnames(leftInfo)
  for(i in 1:length(propertyNames)){
    #Find the property name and check the setting
    curProp=propertyNames[i]
    inheritProp=inherit$extVar[[defineType]][[curProp]]
    if(is.null(inheritProp))
      inheritProp=inherit$extVar[[defineType]]$default
    
    #Check if the property is the same between the left and right expression
    if(inheritProp&&leftInfo[[curProp]]!=rightInfo[[curProp]]){
      curAct=inheritAct[[defineType]][[curProp]][["act"]]
      curWarningLevel=inheritAct[[defineType]][[curProp]][["warningLevel"]]
      if(is.null(curAct))
        curAct="no action"
      if(is.null(curWarningLevel))
        curWarningLevel=switch(curAct,"no action"=0,"version bump"=1,"rename var"=2)
      curAct=switch(curAct,"no action"=0,"version bump"=1,"rename var"=2)
      if(curAct!=0){
        leftInfo[[curProp]]=rightInfo[[curProp]]
      }
      if(curWarningLevel!=0){
        warningLevel=max(warningLevel,curWarningLevel)
      }
      action=max(action,curAct)
      
    }
  }
  
  #Give warning and error when the expression is inside the for and if body
  if("for" %in% level || "if" %in% level){
    if(warningLevel==1){
      warning("The property(s) of the left variable is changed inside the for/if body,\n",
              "The result may be not correct:\n",
              deparse(curExp))
    }
    if(warningLevel==2){
      stop("The definition of the left variable is changed inside the for/if body,\n",
           "Please consider to redefine the variable before it:\n",
           deparse(curExp))
    }
  }
  
  
  #check the precision, if it needs higher precision, then do the version bump
  ######changing the variable type inside the for loop need to be solved in recompiler##########
  requiredPrecision=typeInherit(leftInfo$precisionType,rightInfo$precisionType)
  if(requiredPrecision!=leftInfo$precisionType){
    action=max(action,1)
    leftInfo$precisionType=requiredPrecision
    
    #update the definition
    leftDef=getVarInfo(varInfo,leftVar,version=0)
    leftDef$precisionType=requiredPrecision
    varInfo=setVarInfo(varInfo,leftDef)
  }  
  
  res=list()
  #Version bump
  if(action==1){
    leftInfo$version=leftInfo$version+1
    varInfo=addVarInfo(varInfo,leftInfo)
    versionBump=getVersionBumpCode(leftVar,leftInfo$version)
    extCode=c(extCode,versionBump)
    
    res$varInfo=varInfo
    res$Exp=curExp
    res$extCode=extCode
    return(res)
  }
  #Rename variable
  if(action==2){
    newVar=GPUVar$getTmpVar()
    curExp[[2]]=as.symbol(newVar)
    
    res=profiler_assignment_newVar(level,varInfo,curExp)
    res$Exp=curExp
    res$extCode=c(extCode,res$extCode)
    res$renameList=matrix(c(deparse(leftVar),newVar),1)
    return(res)
  }
  return(list(varInfo=varInfo,extCode=extCode))
  
}

#If the left variable does not exist
#curExp=quote({D=1:gpu_global_id})[[2]]
profiler_assignment_newVar<-function(level,varInfo,curExp){
  leftVar=curExp[[2]]
  rightExp=curExp[[3]]
  
  #Determine if the right expression explicitly or implicitly defines a variable
  defineType="implicit"
  if(is.call(rightExp)&&deparse(rightExp[[1]])%in% .profileExplicitDefine){
    defineType="explicit"
  }
  
  leftInfo=getEmpyTable(type=T_matrix)
  rightInfo=getExpInfo(varInfo,rightExp,errorCheck = T)
  
  propertyNames=colnames(leftInfo)
  for(i in 1:length(propertyNames)){
    #Find the property name and check the setting
    curProp=propertyNames[i]
    inheritProp=inherit$newVar[[defineType]][[curProp]]
    if(is.null(inheritProp))
      inheritProp=inherit$newVar[[defineType]]$default
    
    #Check if the property can be passed to the left expression
    if(inheritProp){
      leftInfo[[curProp]]=rightInfo[[curProp]]
    }
  }
  
  if(!leftInfo$isRef&&!leftInfo$isSeq){
    if(!leftInfo$compileSize1||!leftInfo$compileSize2)
      stop("Unable to determine the matrix size: ",deparse(curExp))
  }
  #add the variable definition
  leftInfo$var=deparse(leftVar)
  
  if(leftInfo$dataType==T_scale)
    leftInfo$location="local"
  
  varInfo=addVarInfo(varInfo,leftInfo)
  
  res=list(varInfo=varInfo)
  return(res)
}


profile_size<-function(varInfo,Exp){
  curInfo=getExpInfo(varInfo,Exp[[2]])
  if(Exp[[1]]=="nrow"){
    ExpInfo=getEmpyTable(T_scale)
    ExpInfo$precisionType=GPUVar$default_index_type
    ExpInfo$value=curInfo$size1
    ExpInfo$compileValue=curInfo$compileSize1
  }
  if(Exp[[1]]=="ncol"){
    ExpInfo=getEmpyTable(T_scale)
    ExpInfo$precisionType=GPUVar$default_index_type
    ExpInfo$value=curInfo$size2
    ExpInfo$compileValue=curInfo$compileSize2
    }
  if(Exp[[1]]=="length"){
    ExpInfo=getEmpyTable(T_scale)
    ExpInfo$precisionType=GPUVar$default_index_type
    ExpInfo$compileValue=curInfo$compileSize1&&curInfo$compileSize2
    if(ExpInfo$compileValue){
      Exp$value=Simplify2(paste0(curInfo$size1,"*",curInfo$size2))
    }
  }
  return(ExpInfo)
  
}



# Exp=parse(text="matrix(10,2)")[[1]]
# Exp=parse(text="matrix(a,2,2)")[[1]]
# Exp=parse(text="matrix(a,2)")[[1]]
# Exp=parse(text="matrix(a,ncol=2)")[[1]]
# Exp=parse(text="matrix(a,2,2)")[[1]]
# Exp=parse(text="matrix(3,2,2)")[[1]]
#matchFunArg(matrix,Exp)


profile_matrix<-function(varInfo,Exp){
  #Get the matrix data and size
  args=matchFunArg(matrix,Exp)
 
  data=args$data
  
  if(is.na(data))
    stop("NA is not supported: ",deparse(Exp))
  
  dataInfo=getExpInfo(varInfo,data)
  rowInfo=getExpInfo(varInfo,args$nrow)
  colInfo=getExpInfo(varInfo,args$ncol)
  
  
  if(rowInfo$dataType!=T_scale||colInfo$dataType!=T_scale){
    stop("The matrix dimension should be a scalar: ",deparse(Exp))
  }
  if(!rowInfo$compileValue||!colInfo$compileValue){
    stop("undetermined matrix size: ",deparse(Exp))
  }
  
  ExpInfo=getEmpyTable()
  ExpInfo$dataType="matrix"
  ExpInfo$compileSize1=TRUE
  ExpInfo$compileSize2=TRUE
  ExpInfo$size1=rowInfo$value
  ExpInfo$size2=colInfo$value
  #The simplification function may simplify the function such that the result is a matrix
  #Therefore the value will not be stored
  ExpInfo$compileValue=FALSE
  ExpInfo
}


# Exp=parse(text="1/100")[[1]]
profile_arithmetic<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  op=Exp[[1]]
  
  leftInfo=getExpInfo(varInfo,leftExp)
  rightInfo=getExpInfo(varInfo,rightExp)
  
  
  if(leftInfo$dataType==T_scale&&rightInfo$dataType==T_scale){
    ExpInfo=getEmpyTable(type=T_scale)
  }else{
    ExpInfo=getEmpyTable(type=T_matrix)
  }
  ExpInfo$precisionType=typeInherit(leftInfo$precisionType,rightInfo$precisionType)
  
  if(op=="/")
    ExpInfo$precisionType=GPUVar$default_float
  
  if(leftInfo$compileSize1&&rightInfo$compileSize1&&leftInfo$compileSize2&&rightInfo$compileSize2){
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
  }
    
  if(ExpInfo$dataType==T_scale&&leftInfo$compileValue==TRUE&&rightInfo$compileValue==TRUE){
    ExpInfo$compileValue=TRUE
    ExpInfo$value=Simplify2(paste0(leftInfo$value,deparse(Exp[[1]]),rightInfo$value))
  }
  
  #Find the right size for the matrix
  if(leftInfo$dataType==T_scale){
    ExpInfo$size1=rightInfo$size1
    ExpInfo$size2=rightInfo$size2
  }else{
    if(rightInfo$dataType==T_scale){
      ExpInfo$size1=leftInfo$size1
      ExpInfo$size2=leftInfo$size2
    }else{
      ExpInfo$size1=Simplify(paste0("max(",leftInfo$size1,",",rightInfo$size1,")"))
      ExpInfo$size2=Simplify(paste0("max(",leftInfo$size2,",",rightInfo$size2,")"))
    }
  }
  
  
  
  errorCheck=setErrorCheck(level="warning",code=deparse(Exp),
                check=paste0("(",leftInfo$size1,"!=",rightInfo$size1,"||",
                             leftInfo$size2,"!=",rightInfo$size2,")",
                             "&&(",leftInfo$size1,"!=1||",leftInfo$size2,"!=1)&&(",
                             rightInfo$size1,"!=1||",rightInfo$size2,"!=1)"),
                msg="Possibly uncomfortable matrix dimension has found")
  
  return(list(ExpInfo=ExpInfo,errorCheck=errorCheck))
}

profile_logical<-function(varInfo,Exp){
  res=profile_arithmetic(varInfo,Exp)
  res$ExpInfo$precisionType="bool"
  res
}



#%*%
profile_matrixMult<-function(varInfo,Exp){
  ExpInfo=getEmpyTable(T_matrix)
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  leftInfo=getExpInfo(varInfo,leftExp)
  rightInfo=getExpInfo(varInfo,rightExp)
  
  ExpInfo$size1=leftInfo$size1
  ExpInfo$size2=rightInfo$size2
  if(leftInfo$compileSize1&&rightInfo$compileSize2){
    ExpInfo$compileSize1=TRUE
    ExpInfo$compileSize2=TRUE
  }
  
  errorCheck=setErrorCheck(level="error",code=deparse(Exp),
                check=paste0(leftInfo$size2,"!=",rightInfo$size1),
                msg="Uncomfortable matrix dimension has found")
  
  list(ExpInfo=ExpInfo,errorCheck=errorCheck)
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
      sub$size=Simplify2(paste0(curInfo$size1,"*",curInfo$size2))
      sub$value=Simplify2(paste0("1:(",curInfo$size1,"*",curInfo$size2,")"))
    }else{
      #two index sub
      sub$compileValue=curInfo[[paste0("compileSize",i)]]
      sub$compileSize=curInfo[[paste0("compileSize",i)]]
      sub$size=curInfo[[paste0("size",i)]]
      sub$value=Simplify2(paste0("1:(",curInfo[[paste0("size",i)]],")"))
    }
    if(sub$size=="1")
      sub$type=T_scale
    else
      sub$type=T_matrix
    
  }else{
      subVarInfo=getExpInfo(varInfo,sub_var)
      sub$compileValue=subVarInfo$compileValue
      sub$compileSize=subVarInfo$compileSize1&&subVarInfo$compileSize2
      sub$value=subVarInfo$value
      sub$size=Simplify2(paste0(subVarInfo$size1,"*",subVarInfo$size2))
      sub$type=ifelse(sub$size=="1",T_scale,T_matrix)
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
      ExpInfo$value=Simplify2(paste0(curInfo$value,"[",sub1$value,",",sub2$value,"]"))
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
      ExpInfo$value=Simplify2(paste0(curInfo$value,"[",sub1$value,"]"))
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
  ExpInfo$value=toCharacter(Exp)
  if(length(grep(".",ExpInfo$value,fixed=T))==0){
    ExpInfo$precisionType=GPUVar$default_int
  }
  ExpInfo$compileValue=TRUE
  ExpInfo$constVal=TRUE
  return(ExpInfo)
}
profile_symbol<-function(varInfo,Exp){
  var_data=getVarInfo(varInfo,Exp)
  var_data
}

profile_floor<-function(varInfo,Exp){
  ExpInfo=getExpInfo(varInfo,Exp[[2]])
  ExpInfo$precisionType=GPUVar$default_int
  return(ExpInfo)
}
profile_ceil<-function(varInfo,Exp){
  ExpInfo=getExpInfo(varInfo,Exp[[2]])
  ExpInfo$precisionType=GPUVar$default_int
  return(ExpInfo)
}

profile_return<-function(varInfo,Exp){
  ExpInfo=getExpInfo(varInfo,Exp[[2]])
  ExpInfo$var=GPUVar$gpu_return_variable
  return(ExpInfo)
}

#Exp=quote(gMatrix(1,10))
profile_gMatrix<-function(varInfo,Exp){
  args=matchFunArg(gMatrix,Exp)
  rowInfo=getExpInfo(varInfo,args$nrow)
  colInfo=getExpInfo(varInfo,args$ncol)
  
  
  if(rowInfo$dataType!=T_scale||colInfo$dataType!=T_scale){
    stop("The matrix dimension should be a scalar: ",deparse(Exp))
  }
  if(!rowInfo$compileValue||!colInfo$compileValue){
    stop("undetermined matrix size: ",deparse(Exp))
  }
  
  ExpInfo=getEmpyTable(1)
  ExpInfo$dataType=T_matrix
  ExpInfo$precisionType=args$precision
  ExpInfo$size1=rowInfo$value
  ExpInfo$size2=colInfo$value
  ExpInfo$constDef=args$constDef
  ExpInfo$compileSize1=TRUE
  ExpInfo$compileSize2=TRUE
  ExpInfo$location=args$location
  ExpInfo$shared=args$shared
  return(ExpInfo)
}

#Exp=quote(gNumber())
profile_gNumber<-function(varInfo,Exp){
  args=matchFunArg(gNumber,Exp)
  ExpInfo=getEmpyTable(T_scale)
  ExpInfo$precisionType=args$precision
  ExpInfo$constDef=args$constDef
  return(ExpInfo)
}

#Exp=quote(subRef(A,tmp))
profile_subRef<-function(varInfo,Exp){
  args=matchFunArg(subRef,Exp)
  curInfo=getExpInfo(varInfo,args$variable)
  if(curInfo$dataType!=T_matrix){
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
  refInfo$isRef=TRUE
  refInfo$ref=code_char
  if(curInfo$isRef){
    refInfo$constVal=TRUE
  }
  refInfo$constDef=TRUE
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

#Exp=quote(seq(1,gpu_global_id))
profile_seq<-function(varInfo,Exp){
  seq<-function(from,to,by=1){}
  args=matchFunArg(seq,Exp)
  fromInfo=getExpInfo(varInfo,args$from)
  toInfo=getExpInfo(varInfo,args$to)
  byInfo=getExpInfo(varInfo,args$by)
  if(fromInfo$dataType!="scale"||toInfo$dataType!="scale"||byInfo$dataType!="scale"){
    stop("The function argument is not a scalar: ",deparse(Exp))
  }
  precision=typeInherit(fromInfo$precisionType,toInfo$precisionType)
  precision=typeInherit(precision,byInfo$precisionType)
  
  expInfo=getEmpyTable()
  expInfo$precisionType=precision
    
  if(fromInfo$compileValue&&toInfo$compileValue&&byInfo$compileValue){
    #expInfo$value=paste0("seq(",fromInfo$value,",",toInfo$value,",",byInfo$value,")")
    expInfo$size1=Simplify2(paste0("floor((",toInfo$value,"-",fromInfo$value,")/",byInfo$value,")+1"))
    expInfo$size2=1
    expInfo$compileSize1=T
    expInfo$compileSize2=T
    #expInfo$compileValue=T
  }
  expInfo$isSeq=T
  expInfo$seq=paste0("seq(",deparse(args$from),",",deparse(args$to),",",deparse(args$by),")")
  expInfo$initialization=FALSE
  
  expInfo
}


#Exp=quote(1:gpu_global_id)
profile_oneStepSeq<-function(varInfo,Exp){
  from=Exp[[2]]
  to=Exp[[3]]
  code=parse(text=paste0("seq(",deparse(from),",",deparse(to),")"))[[1]]
  expInfo=getExpInfo(varInfo,code)
  expInfo
}








