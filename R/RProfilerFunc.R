# curExp=quote({B=A[1]})[[2]]
profiler_assignment_dispatch <- function(level, varInfo, curExp) {
    leftVar = curExp[[2]]
    
    if (hasVar(varInfo, deparse(leftVar))) {
        return(profiler_assignment_exitingVar(level, varInfo, curExp))
    } else {
        return(profiler_assignment_newVar(level, varInfo, curExp))
    }
}

# If the left variable exists
profiler_assignment_exitingVar <- function(level, varInfo, curExp) {
    leftInfoPack = getExpInfo(varInfo, curExp[[2]])
    leftInfo = leftInfoPack$ExpInfo
    rightInfoPack = getExpInfo(varInfo, curExp[[3]])
    rightInfo = rightInfoPack$ExpInfo
    
    res = combineExpInfo(curExp, leftInfoPack, rightInfoPack)
    errorCheck = res$errorCheck
    extCode = res$extCode
    curExp = res$Exp
    
    leftVar = curExp[[2]]
    rightExp = curExp[[3]]
    # TODO: perform the error check here
    
    
    # Determine if the right expression is explicit definition
    if (is.call(rightExp) && deparse(rightExp[[1]]) %in% .profileExplicitDefine) {
        defineType = "explicit"
    } else {
        defineType = "implicit"
    }
    
    # If the left expression is a lazy reference, then do not perform the
    # profiling
    if (leftInfo$constDef || leftInfo$specialType == "ref") {
        return(list(extCode = extCode, Exp = curExp, errorCheck = errorCheck))
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
    
    
    # check the precision, if it needs higher precision, then do the
    # version bump changing the variable type inside the for loop need to
    # be solved in recompiler##########
    requiredPrecision = typeInherit(leftInfo$precisionType, rightInfo$precisionType)
    if (requiredPrecision != leftInfo$precisionType) {
        action = max(action, 1)
        leftInfo$precisionType = requiredPrecision
        
        # update the definition
        leftDef = getVarInfo(varInfo, leftVar, version = 0)
        leftDef$precisionType = requiredPrecision
        varInfo = setVarInfo(varInfo, leftDef)
    }
    
    res = list()
    # Version bump
    if (action == 1) {
        leftInfo$version = leftInfo$version + 1
        varInfo = addVarInfo(varInfo, leftInfo)
        versionBump = getVersionBumpCode(leftVar, leftInfo$version)
        extCode = c(extCode, versionBump)
        
        res$varInfo = varInfo
        res$Exp = curExp
        res$extCode = extCode
        return(res)
    }
    # Rename variable
    if (action == 2) {
        newVar = GPUVar$getTmpVar()
        curExp[[2]] = as.symbol(newVar)
        
        res = profiler_assignment_newVar(level, varInfo, curExp)
        res$Exp = curExp
        res$extCode = c(extCode, res$extCode)
        res$renameList = matrix(c(deparse(leftVar), newVar), 1)
        return(res)
    }
    return(list(varInfo = varInfo, extCode = extCode))
    
}

# If the left variable does not exist
# curExp=quote({D=1:gpu_global_id})[[2]]
profiler_assignment_newVar <- function(level, varInfo, curExp) {
    rightInfoPack = getExpInfo(varInfo, curExp[[3]])
    rightInfo = rightInfoPack$ExpInfo
    
    
    errorCheck = rightInfoPack$errorCheck
    extCode = rightInfoPack$extCode
    curExp[[3]] = rightInfoPack$Exp
    
    leftVar = curExp[[2]]
    rightExp = rightInfoPack$Exp
    
    # Determine if the right expression explicitly or implicitly defines a
    # variable
    if (is.call(rightExp) && deparse(rightExp[[1]]) %in% .profileExplicitDefine) {
        leftInfo = rightInfo
        leftInfo$version = 1
        leftInfo$var = deparse(leftVar)
        varInfo = addVarInfo(varInfo, leftInfo)
        return(list(varInfo = varInfo, extCode = extCode, Exp = curExp, 
            errorCheck = errorCheck))
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
    
    res = list(varInfo = varInfo)
    return(list(varInfo = varInfo, extCode = extCode, Exp = curExp, errorCheck = errorCheck))
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
    ExpInfo$dataType = "matrix"
    ExpInfo$size1 = rowInfo$value
    ExpInfo$size2 = colInfo$value
    # The simplification function may simplify the function such that the
    # result is a matrix Therefore the value will not be stored
    ExpInfo
}


# Exp=parse(text='1/100')[[1]]
profile_arithmetic <- function(varInfo, Exp) {
    leftExp = Exp[[2]]
    rightExp = Exp[[3]]
    op = Exp[[1]]
    
    leftInfoPack = getExpInfo(varInfo, leftExp)
    rightInfoPack = getExpInfo(varInfo, rightExp)
    
    
    leftInfo = leftInfoPack$ExpInfo
    rightInfo = rightInfoPack$ExpInfo
    
    res = combineExpInfo(Exp, leftInfoPack, rightInfoPack)
    
    if (leftInfo$dataType == T_scale && rightInfo$dataType == T_scale) {
        ExpInfo = getEmpyTable(type = T_scale)
    } else {
        ExpInfo = getEmpyTable(type = T_matrix)
    }
    ExpInfo$precisionType = typeInherit(leftInfo$precisionType, rightInfo$precisionType)
    
    if (op == "/") 
        ExpInfo$precisionType = GPUVar$default_float
    
    
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
            ExpInfo$size1 = leftInfo$size1
            ExpInfo$size2 = leftInfo$size2
        }
    }
    
    check = "FALSE"
    if (!isNA(leftInfo$size1) && !isNA(rightInfo$size1)) 
        check = paste0(check, "||(", leftInfo$size1, "!=", rightInfo$size1, 
            ")&&(", leftInfo$size1, "!=1)&&(", rightInfo$size1, "!=1)")
    
    if (!isNA(leftInfo$size2) && !isNA(rightInfo$size2)) 
        check = paste0(check, "||(", leftInfo$size2, "!=", rightInfo$size2, 
            ")&&(", leftInfo$size2, "!=1)&&(", rightInfo$size2, "!=1)")
    
    if (check != "FALSE") 
        check = substr(check, 8, nchar(check))
    errorCheck = setErrorCheck(level = "error", code = deparse(Exp), check = check, 
        msg = "Uncomfortable matrix dimension has been found")
    res$ExpInfo = ExpInfo
    res$errorCheck = rbind(res$errorCheck, errorCheck)
    return(res)
}

profile_logical <- function(varInfo, Exp) {
    res = profile_arithmetic(varInfo, Exp)
    res$ExpInfo$precisionType = "bool"
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
    res = combineExpInfo(Exp, leftInfoPack, rightInfoPack)
    
    ExpInfo$size1 = leftInfo$size1
    ExpInfo$size2 = rightInfo$size2
    
    
    errorCheck = setErrorCheck(level = "error", code = deparse(Exp), check = paste0(leftInfo$size2, 
        "!=", rightInfo$size1), msg = "Uncomfortable matrix dimension has been found")
    
    res$ExpInfo = ExpInfo
    res$errorCheck = errorCheck
    res
}

# i: indicate the subset index number NA: one value subset 1: first
# index 2: second index
getSubInfo <- function(varInfo, curInfo, sub_var, i = NA) {
    sub = list()
    
    if (sub_var == "") {
        # one index sub
        if (is.na(i)) {
            sub$compileValue = TRUE
            sub$compileSize = !isNA(curInfo$size1) && !isNA(curInfo$size2)
            sub$size = Simplify2(paste0(curInfo$size1, "*", curInfo$size2))
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
        sub$size = Simplify2(paste0(subVarInfo$size1, "*", subVarInfo$size2))
        sub$type = ifelse(sub$size == "1", T_scale, T_matrix)
    }
    sub
}

# Exp=quote(A[,ind,drop=TRUE])
profile_subset <- function(varInfo, Exp) {
    # curInfoPack=getVarInfo(varInfo,Exp[[2]])
    curInfo = getVarInfo(varInfo, Exp[[2]])
    args = matchBracketFunc(Exp)
    
    sub1 = list()
    sub2 = list()
    
    # Determine the one sub or two sub
    if (is.null(args$j)) {
        sub1 = getSubInfo(varInfo, curInfo, args$i)
    } else {
        sub1 = getSubInfo(varInfo, curInfo, args$i, 1)
        sub2 = getSubInfo(varInfo, curInfo, args$j, 2)
    }
    
    
    ExpInfo = getEmpyTable(1)
    if (!is.null(args$j)) {
        
        if (sub1$compileValue && sub2$compileValue && !isNA(curInfo$value)) {
            ExpInfo$value = Simplify2(paste0(curInfo$value, "[", sub1$value, 
                ",", sub2$value, "]"))
        }
        if (sub1$compileSize && sub2$compileSize) {
            ExpInfo$size1 = sub1$size
            ExpInfo$size2 = sub2$size
        }
    }
    
    if (is.null(args$j)) {
        if (sub1$compileValue && !isNA(curInfo$value)) {
            ExpInfo$value = Simplify2(paste0(curInfo$value, "[", sub1$value, 
                "]"))
        }
        if (sub1$compileSize) {
            ExpInfo$size1 = sub1$size
            ExpInfo$size2 = 1
        }
    }
    
    ExpInfo$precisionType = curInfo$precisionType
    return(ExpInfo)
}

profile_numeric <- function(Exp) {
    ExpInfo = getEmpyTable(type = T_scale)
    ExpInfo$value = toCharacter(Exp)
    if (length(grep(".", ExpInfo$value, fixed = TRUE)) == 0) {
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

# Exp=quote(gMatrix(1,10))
profile_gMatrix <- function(varInfo, Exp) {
    args = matchFunArg(gMatrix, Exp)
    rowInfoPack = getExpInfo(varInfo, args$nrow)
    colInfoPack = getExpInfo(varInfo, args$ncol)
    
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
    return(ExpInfo)
}

# Exp=quote(gNumber())
profile_gNumber <- function(varInfo, Exp) {
    args = matchFunArg(gNumber, Exp)
    ExpInfo = getEmpyTable(T_scale)
    ExpInfo$precisionType = args$precision
    ExpInfo$constDef = args$constDef
    return(ExpInfo)
}

# Exp=quote(subRef(A,tmp))
profile_subRef <- function(varInfo, Exp) {
    args = matchFunArg(subRef, Exp)
    curInfo = getExpInfo(varInfo, args$variable)$ExpInfo
    if (curInfo$dataType != T_matrix) {
        stop("Only matrix is allow to create a reference: ", deparse(Exp))
    }
    
    if (length(Exp) == 3) {
        code_char = paste0(args$variable, "[", args$i, "]")
    } else {
        code_char = paste0(args$variable, "[", args$i, ",", args$j, "]")
    }
    code = parse(text = code_char)[[1]]
    refInfo = profile_subset(varInfo, code)
    refInfo$designSize1 = 0
    refInfo$designSize2 = 0
    refInfo$initialization = FALSE
    refInfo$isSpecial = TRUE
    refInfo$specialType = "ref"
    refInfo$specialContent = code_char
    if (curInfo$isRef) {
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
    info$ExpInfo = ExpInfo
    
    return(ExpInfo)
}
# Exp=quote(t.nocpy(A))
profile_transpose_nocpy <- function(varInfo, Exp) {
    curVar = Exp[[2]]
    Exp = parse(text = paste0("subRef(", curVar, ",,)"))[[1]]
    info = getExpInfo(varInfo, Exp)
    ExpInfo = info$ExpInfo
    size1 = ExpInfo$size2
    size2 = ExpInfo$size1
    ExpInfo$size1 = size1
    ExpInfo$size2 = size2
    ExpInfo$transpose = !ExpInfo$transpose
    info$ExpInfo = ExpInfo
    
    return(ExpInfo)
}



# Exp=quote(seq(1,gpu_global_id))
profile_seq <- function(varInfo, Exp) {
    seq <- function(from, to, by = 1) {
    }
    args = matchFunArg(seq, Exp)
    fromInfo = getExpInfo(varInfo, args$from)$ExpInfo
    toInfo = getExpInfo(varInfo, args$to)$ExpInfo
    byInfo = getExpInfo(varInfo, args$by)$ExpInfo
    if (fromInfo$dataType != "scale" || toInfo$dataType != "scale" || byInfo$dataType != 
        "scale") {
        stop("The function argument is not a scalar: ", deparse(Exp))
    }
    precision = typeInherit(fromInfo$precisionType, toInfo$precisionType)
    precision = typeInherit(precision, byInfo$precisionType)
    
    expInfo = getEmpyTable()
    expInfo$precisionType = precision
    
    if (!isNA(fromInfo$value) && !isNA(toInfo$value) && !isNA(byInfo$value)) {
        # expInfo$value=paste0('seq(',fromInfo$value,',',toInfo$value,',',byInfo$value,')')
        expInfo$size1 = Simplify2(paste0("floor((", toInfo$value, "-", 
            fromInfo$value, ")/", byInfo$value, ")+1"))
        expInfo$size2 = 1
        # expInfo$compileValue=TRUE
    }
    expInfo$designSize1 = 4
    expInfo$designSize2 = 1
    expInfo$isSpecial = TRUE
    expInfo$location = "local"
    expInfo$shared = FALSE
    expInfo$specialType = "seq"
    expInfo$specialContent = paste0("seq(", deparse(args$from), ",", deparse(args$to), 
        ",", deparse(args$by), ")")
    
    expInfo
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



