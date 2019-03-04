# curExp=quote({B=A[1]})[[2]]
profiler_assignment_dispatch <- function(level, varInfo, curExp) {
    leftVar = curExp[[2]]
    
    if (is.call(leftVar)||hasVar(varInfo, deparse(leftVar))) {
      res=profiler_assignment_exitingVar(level, varInfo, curExp)
    } else {
      res=profiler_assignment_newVar(level, varInfo, curExp)
    }
    return(res)
}

# If the left variable exists
profiler_assignment_exitingVar <- function(level, varInfo, curExp) {
    Exp_record=curExp
    leftInfoPack = getExpInfo(varInfo, curExp[[2]])
    leftInfo = leftInfoPack$ExpInfo
    rightInfoPack = getExpInfo(varInfo, curExp[[3]])
    rightInfo = rightInfoPack$ExpInfo
    result=list(Exp=curExp)
    result = combineExpInfo(result, leftInfoPack, rightInfoPack)
    # errorCheck = res$errorCheck
    # insertBefore = res$insertBefore
    # curExp = res$Exp
    
    curExp=result$Exp
    leftVar = curExp[[2]]
    rightExp = curExp[[3]]
    check=errorCheck_matrix_matrix(
      leftInfo$size1,leftInfo$size2,
      rightInfo$size1,rightInfo$size2)
    errorCheck=setErrorCheck("error",deparse(Exp_record),check,"Uncomfortable matrix dimension is found")
    
    
    
    # Determine if the right expression is explicit definition
    if (is.call(rightExp) && deparse(rightExp[[1]]) %in% .profileExplicitDefine) {
        defineType = "explicit"
    } else {
        defineType = "implicit"
    }
    
    # If the left expression is a lazy reference, then do not perform the
    # profiling
    if (leftInfo$constDef || leftInfo$specialType == "ref") {
        return(result)
    }
    #If the left expression is a function, then check the matrix dimension
    if(is.call(leftVar)){
      left_func=deparse(leftVar[[1]])
      result$errorCheck=rbind(result$errorCheck,errorCheck)
      
      if(!is.null(.profileCheckFuncs[[left_func]])){
        result=.profileCheckFuncs[[left_func]](result,varInfo,curExp,leftInfo,rightInfo)
      }else{
        stop("The left function is not supported: ",deparse(curExp))
      }
      return(result)
    }
    
    if (leftInfo$constVal) {
        stop("The left variable is a constant and cannot be changed: ", 
            deparse(curExp))
    }
    
    
    action = 0
    warningLevel = 0
    
    if (!leftInfo$isSpecial) {
        propertyNames = colnames(leftInfo)
        for (i in seq_along(propertyNames)) {
            # Find the property name and check the setting
            curProp = propertyNames[i]
            inheritProp = inherit$extVar[[defineType]][[curProp]]
            if (is.null(inheritProp)) 
                next
            if(curProp%in%c("size1","size2")&&("for" %in% level || "if" %in% level)){
              if(!isNumeric(leftInfo[[curProp]])&&!isNumeric(rightInfo[[curProp]]))
                next
            }
            # Check if the property is the same between the left and right
            # expression
            if (inheritProp && leftInfo[[curProp]] != rightInfo[[curProp]]) {
                curAct = inheritAct[[defineType]][[curProp]][["act"]]
                curWarningLevel = inheritAct[[defineType]][[curProp]][["warningLevel"]]
                if (is.null(curAct)) 
                  curAct = "no action"
                if (is.null(curWarningLevel)) 
                  curWarningLevel = switch(curAct, `no action` = 0, `version bump` = 1, 
                    `rename var` = 2)
                curAct = switch(curAct, `no action` = 0, `version bump` = 1, 
                  `rename var` = 2)
                if (curAct != 0) {
                  leftInfo[[curProp]] = rightInfo[[curProp]]
                }
                if (curWarningLevel != 0) {
                  warningLevel = max(warningLevel, curWarningLevel)
                }
                action = max(action, curAct)
            }
        }
    } else {
      #Special type
      #Perform special process
        if (leftInfo$specialType == "seq" && rightInfo$specialType != "seq") {
            stop("The sequence variable cannot be changed: ", deparse(curExp))
        }
        if (leftInfo$specialType == "seq" && rightInfo$specialType == "seq") {
            props = c("size1", "size2", "specialContent")
            if (leftInfo[, props] != rightInfo[, props]) {
                action = 1
                leftInfo[, props] = rightInfo[, props]
            }
        }
    }
    
    
    # Give warning and error when the expression is inside the for and if
    # body
    if ("for" %in% level || "if" %in% level) {
        if (warningLevel == 1) {
            warning("The property(s) of the left variable is changed inside the for/if body,\n", 
                "The result may be not correct:\n", deparse(curExp))
        }
        if (warningLevel == 2) {
            stop("The definition of the left variable is changed inside the for/if body,\n", 
                "Please consider to redefine the variable before it:\n", 
                deparse(curExp))
        }
    }
    
    
    # check the precision, if it needs higher precision, then change the precision
    ###########changing the variable type inside the for loop need to be solved in recompiler##########
    requiredPrecision = typeInherit(leftInfo$precisionType, rightInfo$precisionType)
    if (requiredPrecision != leftInfo$precisionType) {
        action = max(action, 1)
        leftInfo$precisionType = requiredPrecision
        
        # update the definition
        leftDef = getVarInfo(varInfo, leftVar)
        leftDef$precisionType = requiredPrecision
        varInfo = setVarInfo(varInfo, leftDef)
    }
    
    # Version bump
    if (action == 1) {
        leftInfo$version = leftInfo$version + 1
        varInfo = addVarInfo(varInfo, leftInfo)
        versionBump = getVersionBumpCode(leftVar, leftInfo$version)
        
        result$varInfo = varInfo
        result$Exp = curExp
        result$insertBefore = c(result$insertBefore, versionBump)
        #If just a version bump, then do the error check
        result$errorCheck=rbind(result$errorCheck,errorCheck)
        return(result)
    }
    # Rename variable
    # No error check if the variable is renamed
    if (action == 2) {
        newVar = GPUVar$getTmpVar()
        curExp[[2]] = as.symbol(newVar)
        
        res = profiler_assignment_newVar(level, varInfo, curExp)
        res$renameList = matrix(c(deparse(leftVar), newVar), 1)
        return(res)
    }
    
    #If no action, then do the error check
    result$errorCheck=rbind(result$errorCheck,errorCheck)
    return(result)
}

# If the left variable does not exist
# curExp=quote({D=1:gpu_global_id})[[2]]
profiler_assignment_newVar <- function(level, varInfo, curExp) {
    result=list(Exp=curExp,varInfo=varInfo)
    
    rightInfoPack = getExpInfo(varInfo, curExp[[3]])
    rightInfo = rightInfoPack$ExpInfo
    
    result=combineExpInfo(result,rightInfoPack,offset = 3,autoOffset = FALSE)
    
    curExp = result$Exp
    
    leftVar = curExp[[2]]
    rightExp = curExp[[3]]
    
    # Determine if the right expression explicitly or implicitly defines a
    # variable
    if (is.call(rightExp) && deparse(rightExp[[1]]) %in% .profileExplicitDefine) {
        leftInfo = rightInfo
        leftInfo$version = 1
        leftInfo$var = deparse(leftVar)
        varInfo = addVarInfo(varInfo, leftInfo)
        
        result$varInfo=varInfo
        return(result)
    }
    
    
    
    leftInfo = getEmpyTable(type = T_matrix)
    inheritedProperty = inherit$newVar$implicit
    leftInfo[, inheritedProperty] = rightInfo[, inheritedProperty]
    
    
    if (!leftInfo$isSpecial) {
        if (isNA(leftInfo$size1) || isNA(leftInfo$size2)) 
            stop("Unable to determine the matrix size: ", deparse(curExp))
    }
    
    # add the variable definition
    leftInfo$var = deparse(leftVar)
    
    if (leftInfo$dataType == T_scale) 
        leftInfo$location = "local"
    
    varInfo = addVarInfo(varInfo, leftInfo)
    
    result$varInfo=varInfo
    return(result)
}


profile_size <- function(varInfo, Exp) {
    curInfoPack = getExpInfo(varInfo, Exp[[2]])
    curInfo = curInfoPack$ExpInfo
    if (Exp[[1]] == "nrow") {
        ExpInfo = getEmpyTable(T_scale)
        ExpInfo$precisionType = GPUVar$default_index_type
        ExpInfo$value = curInfo$size1
    }
    if (Exp[[1]] == "ncol") {
        ExpInfo = getEmpyTable(T_scale)
        ExpInfo$precisionType = GPUVar$default_index_type
        ExpInfo$value = curInfo$size2
    }
    if (Exp[[1]] == "length") {
        ExpInfo = getEmpyTable(T_scale)
        ExpInfo$precisionType = GPUVar$default_index_type
        if (!isNA(curInfo$size1) && !isNA(curInfo$size2)) {
            Exp$value = Simplify2(paste0(curInfo$size1, "*", curInfo$size2))
        }
    }
    return(ExpInfo)
    
}



# Exp=parse(text='matrix(10,2)')[[1]]
# Exp=parse(text='matrix(a,2,2)')[[1]]
# Exp=parse(text='matrix(a,2)')[[1]]
# Exp=parse(text='matrix(a,ncol=2)')[[1]]
# Exp=parse(text='matrix(a,2,2)')[[1]]
# Exp=parse(text='matrix(3,2,2)')[[1]] matchFunArg(matrix,Exp)


profile_matrix <- function(varInfo, Exp) {
    # Get the matrix data and size
    args = matchFunArg(matrix, Exp)
    
    data = args$data
    
    if (isNA(deparse(data))) 
        stop("NA is not supported: ", deparse(Exp))
    Exp_char=paste0("matrix(",args$data,",",args$nrow,",",args$ncol,",",args$byrow,")")
    Exp=parse(text=Exp_char)[[1]]
    
    dataInfoPack = getExpInfo(varInfo, data)
    rowInfoPack = getExpInfo(varInfo, args$nrow)
    colInfoPack = getExpInfo(varInfo, args$ncol)
    #byrowInfoPack = getExpInfo(varInfo, args$byrow)
    
    result=list(Exp=Exp)
    #result=combineExpInfo(result,dataInfoPack,rowInfoPack,colInfoPack,byrowInfoPack)
    result=combineExpInfo(result,dataInfoPack,rowInfoPack,colInfoPack)
    
    
    dataInfo = getExpInfo(varInfo, data)$ExpInfo
    rowInfo = getExpInfo(varInfo, args$nrow)$ExpInfo
    colInfo = getExpInfo(varInfo, args$ncol)$ExpInfo
    
    
    
    
    if (rowInfo$dataType != T_scale || colInfo$dataType != T_scale) {
        stop("The matrix dimension should be a scalar: ", deparse(Exp))
    }
    if (isNA(rowInfo$value) || isNA(colInfo$value)) {
        stop("undetermined matrix size: ", deparse(Exp))
    }
    
    ExpInfo = getEmpyTable()
    #You should not deduct the precision type
    #Because people will use 0 to initialize the matrix
    ExpInfo$precisionType=dataInfo$precisionType
    ExpInfo$dataType = "matrix"
    ExpInfo$size1 = rowInfo$value
    ExpInfo$size2 = colInfo$value
    # The simplification function may simplify the function such that the
    # result is a matrix Therefore the value will not be stored
    result$ExpInfo=ExpInfo
    result
}

profile_elementOP<-function(varInfo, Exp){
  leftExp = Exp[[2]]
  rightExp = Exp[[3]]
  
  leftInfoPack = getExpInfo(varInfo, leftExp)
  rightInfoPack = getExpInfo(varInfo, rightExp)
  
  
  leftInfo = leftInfoPack$ExpInfo
  rightInfo = rightInfoPack$ExpInfo
  
  res = combineExpInfo(list(Exp=Exp), leftInfoPack, rightInfoPack)
  
  if (leftInfo$dataType == T_scale && rightInfo$dataType == T_scale) {
    ExpInfo = getEmpyTable(type = T_scale)
  } else {
    ExpInfo = getEmpyTable(type = T_matrix)
  }
  ExpInfo$precisionType = typeInherit(leftInfo$precisionType, rightInfo$precisionType)
  
  if (ExpInfo$dataType == T_scale) {
    ExpInfo$value = Simplify2(paste0(leftInfo$value, deparse(Exp[[1]]), 
                                     rightInfo$value))
  }
  
  # Find the right size for the matrix
  if (leftInfo$dataType == T_scale) {
    ExpInfo$size1 = rightInfo$size1
    ExpInfo$size2 = rightInfo$size2
  } else {
    if (rightInfo$dataType == T_scale) {
      ExpInfo$size1 = leftInfo$size1
      ExpInfo$size2 = leftInfo$size2
    } else {
      # If two elements are all matrix, use the dimension of the first matrix
      ExpInfo$size1 = Simplify2(
        paste0("max(",leftInfo$size1,",",rightInfo$size1,")"))
      ExpInfo$size2 = Simplify2(
        paste0("max(",leftInfo$size2,",",rightInfo$size2,")"))
    }
  }
  
  check=errorCheck_matrix_matrix(
    leftInfo$size1,leftInfo$size2,
    rightInfo$size1,rightInfo$size2)
  
  errorCheck = setErrorCheck(level = "error", code = deparse(Exp), check = check, 
                             msg = "Uncomfortable matrix dimension is found")
  res$ExpInfo = ExpInfo
  res$errorCheck = rbind(res$errorCheck, errorCheck)
  return(res)
}

#support the element operation function with arbitrary parameters
#Parm index: the parameters that should be considered in element operation
profile_elementOP<-function(varInfo, Exp,parmsIndex=seq_len(length(Exp)-1)+1){
  parmsInfoPack=lapply(parmsIndex,
               function(i,varInfo,Exp) getExpInfo(varInfo, Exp[[i]]),
               varInfo=varInfo,Exp=Exp)
  
  parmsInfo=lapply(parmsInfoPack,function(parm)parm$ExpInfo)
  
  
  previousCheck=lapply(parmsInfoPack,function(x)x$errorCheck)
  errorCheck=c()
  for(i in seq_along(previousCheck)){
    errorCheck=rbind(errorCheck,previousCheck[[i]])
  }
  
  result=list(Exp=Exp,errorCheck=errorCheck)
  isScalar=TRUE
  hasValue=TRUE
  value=c()
  precision="char"
  size1=1
  size2=1
  check="FALSE"
  for(i in seq_along(parmsIndex)){
    result=combineExpInfo(result,parmsInfoPack[[i]],offset=parmsIndex[i],autoOffset = FALSE)
    precision=typeInherit(precision, parmsInfo[[i]]$precisionType)
    if(isNA(parmsInfo[[i]]$value,C=FALSE)) {
      hasValue=FALSE
    }else{
      value=c(value,parmsInfo[[i]]$value)
    }
    if(parmsInfo[[i]]$dataType!=T_scale) {
      if(!isScalar){
        curCheck=errorCheck_matrix_matrix(
          size1,size2,
          parmsInfo[[i]]$size1,parmsInfo[[i]]$size2)
        check=paste0(check,"||(",curCheck,")")
      }
      
      isScalar=FALSE
      if(!isNA(size1,C=FALSE)&&!isNA(parmsInfo[[i]]$size1,C=FALSE)){
        size1=Simplify2(paste0("max(",size1,",",parmsInfo[[i]]$size1,")"))
      }else{
        size1="NA"
      }
      if(!isNA(size2,C=FALSE)&&!isNA(parmsInfo[[i]]$size2,C=FALSE)){
        size2=Simplify2(paste0("max(",size2,",",parmsInfo[[i]]$size2,")"))
      }else{
        size2="NA"
      }
    }
  }
  
  
  if (isScalar) {
    ExpInfo = getEmpyTable(type = T_scale)
  } else {
    ExpInfo = getEmpyTable(type = T_matrix)
  }
  ExpInfo$precisionType = precision
  ExpInfo$size1=size1
  ExpInfo$size2=size2
  
  if(!is.null(check)){
    check=Simplify2(check,parentheses = FALSE)
    errorCheck = setErrorCheck(level = "error", code = deparse(Exp), check = check, 
                               msg = "Uncomfortable matrix dimension is found")
    result$errorCheck = rbind(result$errorCheck, errorCheck)
  }
  result$ExpInfo = ExpInfo
  if(hasValue){
    result$addition=list()
    result$addition$value=value
  }
  return(result)
}


# Exp=parse(text='1/100')[[1]]
profile_arithmetic <- function(varInfo, Exp) {
  op=deparse(Exp[[1]])
  res=profile_elementOP(varInfo, Exp)
  if (op == "/") 
    res$ExpInfo$precisionType =typeInherit(res$ExpInfo$precisionType,GPUVar$default_float)
  
  res$ExpInfo$precisionType =typeTruncate(res$ExpInfo$precisionType)
  if(!is.null(res$addition$value)){
    res$ExpInfo$value=Simplify2(paste0(res$addition$value[1],op,res$addition$value[2]),enhance=FALSE)
  }
  
  return(res)
}

profile_logical <- function(varInfo, Exp) {
    op=deparse(Exp[[1]])
    res = profile_elementOP(varInfo, Exp)
    res$ExpInfo$precisionType = "bool"
    if(!is.null(res$value)){
      res$ExpInfo$value=Simplify2(paste0(res$addition$value[1],op,res$addition$value[2]),enhance=FALSE)
    }
    res
}

profile_abs <- function(varInfo, Exp) {
  res = getExpInfo(varInfo, Exp[[2]])
  precision=res$ExpInfo$precisionType
  if(precision%in%getIntegerType())
     res$Exp=parse(text =paste0("abs_int(",deparse(res$Exp),")") )[[1]]
  else
    res$Exp=parse(text =paste0("abs_float(",deparse(res$Exp),")") )[[1]]
  res
}


# %*%
profile_matrixMult <- function(varInfo, Exp) {
    ExpInfo = getEmpyTable(T_matrix)
    leftExp = Exp[[2]]
    rightExp = Exp[[3]]
    leftInfoPack = getExpInfo(varInfo, leftExp)
    rightInfoPack = getExpInfo(varInfo, rightExp)
    
    leftInfo = leftInfoPack$ExpInfo
    rightInfo = rightInfoPack$ExpInfo
    res = combineExpInfo(list(Exp=Exp), leftInfoPack, rightInfoPack)
    
    ExpInfo$size1 = leftInfo$size1
    ExpInfo$size2 = rightInfo$size2
    
    
    errorCheck = setErrorCheck(level = "error", code = deparse(Exp), check = paste0(leftInfo$size2, 
        "!=", rightInfo$size1), msg = "Uncomfortable matrix dimension is found")
    
    res$ExpInfo = ExpInfo
    res$errorCheck = rbind(res$errorCheck, errorCheck)
    res
}

profile_parenthesis<-function(varInfo, Exp){
  info=getExpInfo(varInfo, Exp[[2]])
  info$Exp=parse(text=paste0("(",deparse(info$Exp),")"))[[1]]
  return(info)
}


# i: indicate the subset index number 
# NA: one value subset 
# 1: first index 
# 2: second index
getSubInfo <- function(varInfo, curInfo, sub_var, i = NA) {
    sub = list()
    subVarInfoPack=NULL
    if (sub_var == "") {
        # one index sub
        if (is.na(i)) {
            sub$compileValue = TRUE
            sub$compileSize = !isNA(curInfo$size1) && !isNA(curInfo$size2)
            sub$size = Simplify2(paste0(curInfo$size1, "*", curInfo$size2),enhance = FALSE)
            sub$value = ""
        } else {
            # two index sub
            sub$compileValue = TRUE
            sub$compileSize = !isNA(curInfo[[paste0("size", i)]])
            sub$size = curInfo[[paste0("size", i)]]
            sub$value = ""
        }
        if (sub$size == "1") 
            sub$type = T_scale else sub$type = T_matrix
        
    } else {
        subVarInfoPack = getExpInfo(varInfo, sub_var)
        subVarInfo = subVarInfoPack$ExpInfo
        
        sub$compileValue = !isNA(subVarInfo$value)
        sub$compileSize = !isNA(subVarInfo$size1) && !isNA(subVarInfo$size2)
        sub$value = subVarInfo$value
        sub$size = Simplify2(paste0(subVarInfo$size1, "*", subVarInfo$size2),enhance = FALSE)
        sub$type = ifelse(sub$size == "1", T_scale, T_matrix)
    }
    list(sub=sub,InfoPack=subVarInfoPack)
}

# Exp=quote(A[ind,drop=TRUE])
profile_subset <- function(varInfo, Exp) {
    curInfoPack=getExpInfo(varInfo,Exp[[2]])
    curInfo = curInfoPack$ExpInfo
    args = matchBracketFunc(Exp)
    args_char=lapply(args,deparse)
    ExpInfo = getEmpyTable(1)
    
    if(is.null(args$j)){
      Exp=parse(text=paste0(deparse(curInfoPack$Exp),"[",args_char$i,"]"))[[1]]
      sub1Pack = getSubInfo(varInfo, curInfo, args$i)
      sub1=sub1Pack$sub
      if (sub1$compileValue && !isNA(curInfo$value)) {
        ExpInfo$value = Simplify2(paste0(curInfo$value, "[", sub1$value, 
                                         "]"))
      }
      if (sub1$compileSize) {
        ExpInfo$size1 = sub1$size
        ExpInfo$size2 = 1
      }
      result=list(Exp=Exp)
      result=combineExpInfo(result,curInfoPack,sub1Pack$InfoPack)
    }else{
      Exp=parse(text=paste0(deparse(curInfoPack$Exp),"[",args_char$i,",",args_char$j,"]"))[[1]]
      sub1Pack = getSubInfo(varInfo, curInfo, args$i, 1)
      sub2Pack = getSubInfo(varInfo, curInfo, args$j, 2)
      sub1=sub1Pack$sub
      sub2=sub2Pack$sub
      if (sub1$compileValue && sub2$compileValue && !isNA(curInfo$value)) {
        ExpInfo$value = Simplify2(paste0(curInfo$value, "[", sub1$value, 
                                         ",", sub2$value, "]"))
      }
      if (sub1$compileSize && sub2$compileSize) {
        ExpInfo$size1 = sub1$size
        ExpInfo$size2 = sub2$size
      }
      result=list(Exp=Exp)
      result=combineExpInfo(result,curInfoPack,sub1Pack$InfoPack,sub2Pack$InfoPack)
    }
    
    
    ExpInfo$precisionType = curInfo$precisionType
    result$ExpInfo=ExpInfo
    return(ExpInfo)
}

profile_numeric <- function(Exp) {
    ExpInfo = getEmpyTable(type = T_scale)
    ExpInfo$value = toCharacter(Exp)
    if (is.wholenumber(Exp)) {
        ExpInfo$precisionType = GPUVar$default_int
    }
    ExpInfo$constVal = TRUE
    return(ExpInfo)
}
profile_symbol <- function(varInfo, Exp) {
    var_data = getVarInfo(varInfo, Exp)
    var_data
}

profile_floor <- function(varInfo, Exp) {
    ExpInfoPack = getExpInfo(varInfo, Exp[[2]])
    ExpInfoPack$Exp = parse(text = paste0("floor(", deparse(ExpInfoPack$Exp), 
        ")"))[[1]]
    ExpInfoPack$ExpInfo$precisionType = GPUVar$default_int
    return(ExpInfoPack)
}
profile_ceil <- function(varInfo, Exp) {
    ExpInfoPack = getExpInfo(varInfo, Exp[[2]])
    ExpInfoPack$Exp = parse(text = paste0("ceiling(", deparse(ExpInfoPack$Exp), 
        ")"))[[1]]
    ExpInfoPack$ExpInfo$precisionType = GPUVar$default_int
    return(ExpInfoPack)
}

profile_return <- function(varInfo, Exp) {
    ExpInfoPack = getExpInfo(varInfo, Exp[[2]])
    # ExpInfo$var=GPUVar$gpu_return_variable
    return(ExpInfoPack)
}

# Exp=quote(Matrix(1,10))
profile_Matrix <- function(varInfo, Exp) {
    args = matchFunArg(Matrix, Exp)
    rowInfoPack = getExpInfo(varInfo, args$nrow)
    colInfoPack = getExpInfo(varInfo, args$ncol)
    result=list(Exp=Exp)
    result=combineExpInfo(result,rowInfoPack,colInfoPack,offset=1,autoOffset = FALSE)
    
    rowInfo = rowInfoPack$ExpInfo
    colInfo = colInfoPack$ExpInfo
    
    if (rowInfo$dataType != T_scale || colInfo$dataType != T_scale) {
        stop("The matrix dimension should be a scalar: ", deparse(Exp))
    }
    if (isNA(rowInfo$value) || isNA(colInfo$value)) {
        stop("undetermined matrix size: ", deparse(Exp))
    }
    
    ExpInfo = getEmpyTable(1)
    ExpInfo$dataType = T_matrix
    ExpInfo$precisionType = args$precision
    ExpInfo$size1 = rowInfo$value
    ExpInfo$size2 = colInfo$value
    ExpInfo$constDef = args$constDef
    ExpInfo$location = args$location
    ExpInfo$shared = args$shared
    
    result$ExpInfo=ExpInfo
    return(result)
}

# Exp=quote(Scalar())
profile_Scalar <- function(varInfo, Exp) {
    args = matchFunArg(Scalar, Exp)
    ExpInfo = getEmpyTable(T_scale)
    ExpInfo$precisionType = args$precision
    ExpInfo$constDef = args$constDef
    return(ExpInfo)
}

# Exp=quote(subRef(A,tmp))
profile_subRef <- function(varInfo, Exp) {
    args = matchFunArg(subRef, Exp)
    curInfo = getExpInfo(varInfo, args$variable)$ExpInfo
    # if (curInfo$dataType != T_matrix) {
    #     stop("Only matrix is allow to create a reference: ", deparse(Exp))
    # }
    
    if (length(Exp) == 3) {
        code_char = paste0(args$variable, "[", args$i, "]")
    } else {
        code_char = paste0(args$variable, "[", args$i, ",", args$j, "]")
    }
    code = parse(text = code_char)[[1]]
    refInfo = profile_subset(varInfo, code)
    refInfo$designSize = 0
    refInfo$initial_ad = FALSE
    refInfo$isSpecial = TRUE
    refInfo$specialType = "ref"
    refInfo$specialContent = code_char
    if (curInfo$constVal) {
        refInfo$constVal = TRUE
    }
    refInfo$constDef = TRUE
    refInfo
}



# This function only process the copy transform Exp=quote(t(A))
profile_transpose <- function(varInfo, Exp) {
    info = getExpInfo(varInfo, Exp[[2]])
    ExpInfo = info$ExpInfo
    size1 = ExpInfo$size2
    size2 = ExpInfo$size1
    ExpInfo$size1 = size1
    ExpInfo$size2 = size2
    result=list(Exp=Exp,ExpInfo=ExpInfo)
    result=combineExpInfo(result,info)
    return(result)
}
# Exp=quote(t_nocpy(A))
profile_transpose_nocpy <- function(varInfo, Exp) {
    curVar = Exp[[2]]
    Exp_new = parse(text = paste0("subRef(", curVar, ",,)"))[[1]]
    info = getExpInfo(varInfo, Exp_new)
    ExpInfo = info$ExpInfo
    size1 = ExpInfo$size2
    size2 = ExpInfo$size1
    ExpInfo$size1 = size1
    ExpInfo$size2 = size2
    ExpInfo$transpose = !ExpInfo$transpose
    info$ExpInfo = ExpInfo
    info$Exp=Exp
    
    return(info)
}



# Exp=quote(seq(1,gpu_global_id,length.out=5))
profile_seq <- function(varInfo, Exp) {
    seq <- function(from, to, by=NULL ,length.out=NULL) {
    }
    Exp=standardise_call(Exp)
    args = matchFunArg(seq, Exp)
    fromInfoPack=getExpInfo(varInfo, args$from)
    toInfoPack=getExpInfo(varInfo, args$to)
    
    fromInfo = fromInfoPack$ExpInfo
    toInfo = toInfoPack$ExpInfo
    ExpInfo = getEmpyTable()
    res=list(Exp=Exp)
    if(is.null(args$length.out)){
      if(is.null(args$by)){
        byInfo = getEmpyTable(T_scale)
        byInfo$precisionType=GPUVar$default_int
        if (!isNA(fromInfo$value) && !isNA(toInfo$value))
          byInfo$value=paste0("sign(",toInfo$value,"-",fromInfo$value,")")
      }else{
        byInfoPack=getExpInfo(varInfo, args$by)
        byInfo = byInfoPack$ExpInfo
        res=combineExpInfo(res,fromInfoPack,toInfoPack,byInfoPack)
      }
      
      if (fromInfo$dataType != T_scale || toInfo$dataType != T_scale || byInfo$dataType != 
          T_scale) {
        stop("The function argument is not a scalar: ", deparse(Exp))
      }
      precision = typeInherit(fromInfo$precisionType, toInfo$precisionType)
      precision = typeInherit(precision, byInfo$precisionType)
      
      if (!isNA(fromInfo$value) && !isNA(toInfo$value) && !isNA(byInfo$value)) {
        ExpInfo$size1 = Simplify2(paste0("abs(trunc((", toInfo$value, "-", 
                                         fromInfo$value, ")/(", byInfo$value, ")))+1"))
      }
    }else{
      #If the length.out has been specified
      lengthInfoPack=getExpInfo(varInfo, args$length.out)
      lengthInfo=lengthInfoPack$ExpInfo
      res=combineExpInfo(res,fromInfoPack,toInfoPack,lengthInfoPack)
      if (fromInfo$dataType != T_scale || toInfo$dataType != T_scale || lengthInfo$dataType != 
          T_scale) {
        stop("The function argument is not a scalar: ", deparse(Exp))
      }
      lengthType=GPUVar$default_float
      if(isNumeric(fromInfo$value)&&isNumeric(toInfo$value)&&isNumeric(lengthInfo$value)){
        byInfo=Simplify(paste0("(", toInfo$value, "-", 
                                         fromInfo$value, ")/(",lengthInfo$value,"-1)"))
        if(is.wholenumber(as.numeric(byInfo))&&
           is.wholenumber(as.numeric(fromInfo$value))){
          lengthType=GPUVar$default_int
        }
      }
      precision = typeInherit(fromInfo$precisionType, toInfo$precisionType)
      precision = typeInherit(precision, lengthType)
      if(!isNA(lengthInfo$value)){
        ExpInfo$size1=lengthInfo$value
      }
    }
    ExpInfo$precisionType = precision
    ExpInfo$size2 = 1
    ExpInfo$designSize = 3
    ExpInfo$isSpecial = TRUE
    ExpInfo$location = "local"
    ExpInfo$shared = FALSE
    ExpInfo$specialType = "seq"
    ExpInfo$specialContent = paste0("seq(", deparse(args$from), ",", deparse(args$to), 
        ",", deparse(args$by),",",deparse(args$length.out), ")")
    
    
    res$ExpInfo=ExpInfo
    res
}


# Exp=quote(1:gpu_global_id)
profile_oneStepSeq <- function(varInfo, Exp) {
    from = Exp[[2]]
    to = Exp[[3]]
    code = parse(text = paste0("seq(", deparse(from), ",", deparse(to), 
        ")"))[[1]]
    expInfo = getExpInfo(varInfo, code)
    expInfo
}
# Exp=quote(sum(A))
profile_sum <- function(varInfo, Exp) {
    res = getExpInfo(varInfo,Exp[[2]])
    ExpInfo = res$ExpInfo
    curInfo = getEmpyTable(T_scale)
    curInfo$precisionType = typeTruncate(ExpInfo$precisionType)
    res$ExpInfo = curInfo
    res$Exp=parse(text=paste0("sum(",deparse(res$Exp),")"))[[1]]
    return(res)
}

profile_rowSums <- function(varInfo, Exp) {
  res = getExpInfo(varInfo,Exp[[2]])
  ExpInfo = res$ExpInfo
  curInfo = getEmpyTable()
  curInfo$precisionType = typeTruncate(ExpInfo$precisionType)
  curInfo$size1=ExpInfo$size1
  curInfo$size2=1
  res$ExpInfo = curInfo
  res$Exp=parse(text=paste0("rowSums(",deparse(res$Exp),")"))[[1]]
  return(res)
}
profile_colSums <- function(varInfo, Exp) {
  res = getExpInfo(varInfo,Exp[[2]])
  ExpInfo = res$ExpInfo
  curInfo = getEmpyTable()
  curInfo$precisionType = typeTruncate(ExpInfo$precisionType)
  curInfo$size1=ExpInfo$size2
  curInfo$size2=1
  res$ExpInfo = curInfo
  res$Exp=parse(text=paste0("colSums(",deparse(res$Exp),")"))[[1]]
  return(res)
}

profile_rowMeans <- function(varInfo, Exp) {
  res = profile_rowSums(varInfo, Exp)
  res$ExpInfo$precisionType=typeInherit(res$ExpInfo$precisionType,GPUVar$default_float)
  res$Exp[[1]]=as.symbol("rowMeans")
  return(res)
}

profile_colMeans <- function(varInfo, Exp) {
  res = profile_colSums(varInfo, Exp)
  res$ExpInfo$precisionType=typeInherit(res$ExpInfo$precisionType,GPUVar$default_float)
  res$Exp[[1]]=as.symbol("colMeans")
  return(res)
}
#Exp=quote(sweep(B+1,2,D,"-",test="a"))
profile_sweep<-function(varInfo,Exp){
  args=matchFunArg(sweep,Exp)
  xInfoPack=getExpInfo(varInfo,args$x)
  marginInfoPack=getExpInfo(varInfo,args$MARGIN)
  statsInfoPack=getExpInfo(varInfo,args$STATS)
  fun=args$FUN
  
  if(!toCharacter(fun)%in%.elementOp){
    stop("Only element operation is allowed in the FUN argument")
  }
  if(!isNumeric(marginInfoPack$ExpInfo$value)){
    stop("Unable to determine the MARGIN parameter: ",deparse(Exp))
  }
  argNames=formalArgs(sweep)
  args[names(args)%in% argNames]=NULL
  curExp=reconstructExp(funcName="sweep",x=xInfoPack$Exp,
                        MARGIN=marginInfoPack$ExpInfo$value,
                        STATS=statsInfoPack$Exp,
                        FUN=toCharacter(fun),dotParms = args)
  res=combineExpInfo(list(Exp=curExp),xInfoPack,marginInfoPack,statsInfoPack)
  xInfo=xInfoPack$ExpInfo
  statsInfo=statsInfoPack$ExpInfo
  res$ExpInfo=xInfo
  res$ExpInfo$precisionType = typeInherit(xInfo$precisionType,statsInfo$precisionType)
  #Perform the error check
  if(as.numeric(marginInfoPack$ExpInfo$value)==1){
    check=errorCheck_matrix_matrix_oneside(xInfo$size1,statsInfo$size1)
    check=paste0(check,"||",errorCheck_matrix_matrix_oneside(statsInfo$size2,1))
  }else{
    if(as.numeric(marginInfoPack$ExpInfo$value)==2){
      check=errorCheck_matrix_matrix_oneside(xInfo$size2,statsInfo$size2)
      check=paste0(check,"||",errorCheck_matrix_matrix_oneside(statsInfo$size1,1))
    }else{
      stop("Invalid MARGIN argument: ",deparse(Exp))
    }
  }
  
  res$errorCheck=rbind(res$errorCheck,
                       setErrorCheck("error",deparse(Exp),check,"Uncomfortable matrix dimension"))
  return(res)
}


#########################Special functions######################################

profile_selfTranspose<-function(varInfo,curExp){
  curVar = curExp[[2]]
  curInfo = getVarInfo(varInfo, curVar)
  # Check if the target can be changed
  if (getVarProperty(varInfo, curVar, "constVal")) {
    stop("The const value cannot be changed", deparse(curExp))
  }
  # set the transpose
  curInfo$version = curInfo$version + 1
  bumpCode = getVersionBumpCode(curVar, curInfo$version)
  tmp = curInfo$size1
  curInfo$size1 = curInfo$size2
  curInfo$size2 = tmp
  curInfo$transpose = curInfo$transpose
  varInfo = addVarInfo(varInfo, curInfo)
  
  
  result$varInfo = varInfo
  result$insertBefore = bumpCode
  result$Exp = curExp[[3]][[1]] = as.symbol("t_nocpy")
  return(result)
}






#=====================profile check left expression========================
findSubsetRoot<-function(Exp){
  if(is.call(Exp)&&Exp[[1]]=="[")
    return(Exp[[2]])
  return(Exp)
}

profileCheck_subset<-function(result,varInfo,Exp,leftInfo,rightInfo){
  leftVar=findSubsetRoot(Exp[[2]])
  if(!is.symbol(leftVar)){
    stop("The left expression is not valid: ",deparse(Exp))
  }
  leftInfo=getVarInfo(varInfo,leftVar)
  if(leftInfo$constDef) return(result)
  targetPrecision=typeInherit(leftInfo$precisionType,rightInfo$precisionType)
  if(leftInfo$precisionType!=targetPrecision){
    leftInfo$precisionType=targetPrecision
    varInfo = setVarInfo(varInfo, leftInfo)
    #leftInfo$version=leftInfo$version+1
    #varInfo = addVarInfo(varInfo, leftInfo)
    #versionBump = getVersionBumpCode(leftVar, leftInfo$version)
    
  }
  result$varInfo = varInfo
  #result$insertBefore = c(result$insertBefore,versionBump)
  return(result)
}

profileCheck_size<-function(result,varInfo,Exp,leftInfo,rightInfo){
  leftExp=Exp[[2]]
  leftVar=leftExp[[2]]
  sizeFunc=leftExp[[1]]
  if(sizeFunc=="nrow"){
    sizeName="size1"
    dynSizeName="dynSize1"
  }else{
    sizeName="size2"
    dynSizeName="dynSize2"
  }
  if(!is.symbol(leftVar)){
    stop("The left expression is not valid: ",deparse(Exp))
  }
  leftInfo=getVarInfo(varInfo,leftVar)
  if(leftInfo$constDef){
    stop("The left variable is a constant: ",deparse(Exp))
  }
  if(leftInfo[[sizeName]]!=rightInfo[[sizeName]]){
    leftInfo[[sizeName]]=rightInfo[[sizeName]]
    leftInfo[[dynSizeName]]=TRUE
    leftInfo$version=leftInfo$version+1
    varInfo = addVarInfo(varInfo, leftInfo)
    versionBump = getVersionBumpCode(leftVar, leftInfo$version)
  }
  result$varInfo = varInfo
  result$insertBefore = c(result$insertBefore,versionBump)
}
