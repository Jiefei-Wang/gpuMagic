
# ===========================profiler 1========================
getDim<-function(x){
  if(is.vector(x)){
    return(c(length(x),1))
  }else{
    return(dim(x))
  }
}
#Generate match rule for determining if the code needs to be recompile
#The function also generate the parameter size table
processDimTbl<-function(parms, parmsWithValue){
  parmsTblName=GPUVar$parmsTblName
  #Find the rule for the parameters with values
  rule=data.frame(var=character(0),matchFunc=character(0),value=character(0),stringsAsFactors = FALSE)
  for(var in parmsWithValue){
    curDim=getDim(parms[[var]])
    curRule=c(curDim[1],curDim[2],
              paste0("matrix(c(",paste0(as.vector(parms[[var]]),collapse = ","),"),nrow=",curDim[1],"))"))
    matchFunc=c("row","col","value")
    
    rule=rbind(rule,cbind(var,matchFunc,curRule),stringsAsFactors =FALSE)
  }
  #Find the variable size for the rest variables
  allVars_ind=which(!names(parms)%in%parmsWithValue)
  nvar=length(allVars_ind)
  varDims=data.frame(var=rep(NA,nvar),
                     ind=0,size1=0,size2=0,
                     size1_char="",size2_char="",stringsAsFactors = FALSE)
  for(i in seq_along(allVars_ind)){
    curVar_ind=allVars_ind[i]
    curDim=getDim(parms[[curVar_ind]])
    varDims[i,]$var=names(parms)[curVar_ind]
    varDims[i,]$ind=curVar_ind
    varDims[i,]$size1=curDim[1]
    varDims[i,]$size2=curDim[2]
  }
  #Find the unique size
  size_list=c(varDims$size1,varDims$size2)
  size_unique_ind=which(!duplicated(size_list))
  size_unique=size_list[size_unique_ind]
  size_unique_var_ind=(size_unique_ind-1)%%nvar+1
  size_unique_var_size_ind=floor((size_unique_ind-1)/nvar+1)
  size_unique_list=data.frame(size=size_unique,var_ind=size_unique_var_ind,size_ind=size_unique_var_size_ind)
  #fill the dimension table and generate the match rule
  for(i in seq_along(allVars_ind)){
    curVar_info=varDims[i,]
    curVar_ind=curVar_info$ind
    curVar_name=curVar_info$var
    for(j in seq_len(2)){
      curRule=NULL
      curSize=as.numeric(curVar_info[2+j])
      prefix=ifelse(j==1,"nrow","ncol")
      if(curSize==1){
        curVar_info[4+j]=1
        curRule=c(curVar_name,prefix,"1")
      }else{
        ind=which(size_unique_list$size==curSize)
        matched_var=size_unique_list$var_ind[ind]
        matched_var_ind=varDims[matched_var,]$ind
        matched_var_name=varDims[matched_var,]$var
        matched_size_ind=size_unique_list$size_ind[ind]
        prefix_matched=ifelse(matched_size_ind==1,"nrow","ncol")
        if(matched_var==i&&matched_size_ind==j){
          #If the matched data is itself, put the size info in the table
          curVar_info[4+j]=paste0(prefix,"(parms[[",curVar_ind,"]])")
        }else{
          #If not, put the matched size into the table and add the rule to check it
          curVar_info[4+j]=paste0(prefix_matched,"(parms[[",matched_var_ind,"]])")
          curRule=c(curVar_name,paste0(prefix,"-",prefix_matched),matched_var_name)
        }
      }
      if(!is.null(curRule)){
        rule=rbind(rule,curRule,stringsAsFactors=FALSE)
      }
    }
    varDims[i,]=curVar_info
  }
  colnames(rule)=c("var","matchFunc","value")
  
  return(list(dimTbl=varDims,rule=rule))
}
# Profile a parameter and give the profile table back
profileVar <- function(parms, parmsWithValue,parmsConst) {
    varInfo = getEmpVarInfoTbl()
    parmsTblName=GPUVar$parmsTblName
    dimRes=processDimTbl(parms,parmsWithValue)
    dimTbl=dimRes$dimTbl
    matchRule=dimRes$rule
    
    varInfo$requiredVar = names(parms)
    
    varName = names(parms)
    for (i in seq_len(length(parms))) {
        info = getEmpyTable()
        info$var = varName[i]
        
        if (is(parms[[i]],"gpuMatrix")) {
          curPrecision = .type(parms[[i]])
        } else {
          curPrecision = GPUVar$default_float
        }
        
        
        info$dataType = T_matrix
        info$precisionType = curPrecision
        info$shared = TRUE
        info$constVal = varName[i] %in% parmsConst
        info$require = TRUE
        info$initial_ad = FALSE
        
        
        curDimInd=which(dimTbl$var==info$var)
        if(length(curDimInd)==0){
          curDim=getDim(parms[[info$var]])
          info$size1=curDim[1]
          info$size2=curDim[2]
        }else{
          info$size1=dimTbl$size1_char[curDimInd]
          info$size2=dimTbl$size2_char[curDimInd]
        }
        
        
        
        if (varName[i] %in% parmsWithValue) {
            info$value = paste0("(", parmsTblName, "[[", i, "]])")
        }
        
        varInfo = addVarInfo(varInfo, info)
    }
    list(varInfo=varInfo,matchRule=matchRule)
}

# ==================================Profiler
# 2==========================
# Exp=quote(Matrix(1,1))
# Find the function parameters If the functions' argument does not show
# in the expression, the default value will be used
matchFunArg <- function(fun, Exp) {
    funArg = formals(fun)
    eval(parse(text = paste0(deparse(Exp[[1]]), "=fun")))
    ExpArg = standardise_call(Exp)
    if (length(ExpArg) > 1) {
        argName = names(ExpArg)
        for (i in 2:length(ExpArg)) {
            funArg[[argName[i]]] = ExpArg[[i]]
        }
    }
    for (i in seq_along(funArg)) {
        if (deparse(funArg[[i]]) == "") 
            funArg[[i]] = NA
        #obtain the default setting
        if (is.call(funArg[[i]])&&!names(funArg)[i]%in%names(ExpArg)) 
            funArg[[i]] = eval(funArg[[i]])
    }
    return(funArg)
}

# Get the right expression profile Return value would be a list with
# the following elements: 
# ExpInfo:the expression info 
# Exp: the expression 
# errorCheck: The error check item 
# insertBefore: the extra code insert before the current expression
# that needs to be added before the expression
getExpInfo <- function(varInfo, Exp) {
    res = getExpInfo_hidden(varInfo, Exp)
    if (is.data.frame(res)) {
        res = list(ExpInfo = res, Exp = Exp)
    }
    if (is.null(res[["Exp"]])) 
        res[["Exp"]] = Exp
    res$ExpInfo$size1=Simplify2(res$ExpInfo$size1)
    res$ExpInfo$size2=Simplify2(res$ExpInfo$size2)
    
    ExpInfo = res$ExpInfo
    #Remove the temporary variable
    res$addition=NULL
    
    # If the variable is explicit definition
    if (is.call(Exp) && (deparse(Exp[[1]]) %in% .profileExplicitDefine)) 
        return(res)
    
    # Some optimization
    if (!isNA(ExpInfo$size1) && !isNA(ExpInfo$size2) && Simplify(ExpInfo$size1) == 
        "1" && Simplify(ExpInfo$size2) == "1") {
        ExpInfo$dataType = T_scale
        ExpInfo$size1 = 1
        ExpInfo$size2 = 1
    }
    if (ExpInfo$dataType == T_scale) 
        ExpInfo$location = "local"
    
    res$ExpInfo = ExpInfo
    return(res)
}
getExpInfo_hidden <- function(varInfo, Exp) {
    if (isNumeric(Exp)) {
        ExpInfo = profile_numeric(Exp)
        return(ExpInfo)
    }
    # If the expression is a function call
    if (is.call(Exp)) {
        func = deparse(Exp[[1]])
        if (!is.null(.profileFuncs[[func]])) {
            ExpInfo = .profileFuncs[[func]](varInfo, Exp)
            return(ExpInfo)
        }
        stop("Unsupported function: ", deparse(Exp))
    }
    
    # If not the above case, the expression will be treated as a variable
    if (is.symbol(Exp)) {
        ExpInfo = profile_symbol(varInfo, Exp)
        return(ExpInfo)
    }
    
    
    stop("Unknow code: ", deparse(Exp))
}


# combine the expression info from several expInfo
combineExpInfo <- function(result, ...,infoPack=NULL,offset=0,autoOffset=TRUE) {
  if(is.language(result))stop("Incorrect old code")
  result=combineInsertCode(result, ...,infoPack=infoPack,offset=offset,autoOffset=autoOffset)
  if(is.null(infoPack)){
    parms = list(...)
  }else{
    parms = infoPack
  }
  for (i in seq_along(parms)) {
    curInfo = parms[[i]]
    result$errorCheck = rbind(result$errorCheck, curInfo$errorCheck)
  }
  return(result)
}


# Format a single code
formatCall <- function(Exp, generalType = FALSE) {
    if (is.numeric(Exp)) {
        if (generalType) 
            return(as.symbol("gType")) else return(as.symbol("num"))
    }
    if (!is.call(Exp)) {
        if (generalType) 
            return(as.symbol("gType")) else return(as.symbol("var"))
    }
    if (length(Exp) > 1) {
        for (i in 2:length(Exp)) {
            Exp[[i]] = formatCall(Exp[[i]], generalType)
        }
    }
    Exp
}


deparse<-function(x){
  base::deparse(x,width.cutoff=500L)
}

# get the version bump code var: the variable name version: the version
# that should be bumped to
getVersionBumpCode <- function(var, version) {
    var_char = toCharacter(var)
    parse(text = paste0(GPUVar$preservedFuncPrefix, "setVersion(", var_char, 
        ",", version, ")"))[[1]]
}
# Add the error check into the varInfo level: the error level: warning,
# error code: The code that generate this error check check: the
# condition that will throw the error(check=TRUE will throw the error)
# msg: the message that will be post when the error occurs
setErrorCheck <- function(level, code, check, msg = "") {
    data.frame(level = level, code = code, check = check, msg = msg, stringsAsFactors = FALSE)
}

#Error check for matrix to matrix operation
#allowed type:
#scalar-scalar
#matrix-scalar
#scalar-matrix
#matrix-matrix with the same dimension
errorCheck_matrix_matrix<-function(left_size1,left_size2,right_size1,right_size2){
  if (!isNA(left_size1) && !isNA(right_size1)&&
      !isNA(left_size2) && !isNA(right_size2)){ 
    #Check row
    check = paste0("((", left_size1, "!=", right_size1,")")
    #Check column
    check = paste0(check,
                   "||(", left_size2, "!=", right_size2,"))")
    #Check if one of them is an scalar
    check=paste0(check,
                 "&&!(", left_size1, "==1&&",left_size2,"==1)&&",
                 "!(", right_size1, "==1&&",right_size2, "==1)")
  }else{
    check="FALSE"
  }
  check=Simplify2(check,parentheses=FALSE)
  check
}

# Redirect the variable to an exist variable to save the memory space
redirectVar <- function(varInfo, sourceVar, desVar) {
    sourceVarInfo = getVarInfo(varInfo, sourceVar)
    if (hasVar(varInfo, desVar)) {
        desVarInfo = getVarInfo(varInfo, desVar)
        # If the destination is a lazy ref or seq object, no redirection is
        # available
        if (desVarInfo$isRef || desVarInfo$isSeq) {
            return()
        }
        # Check if the variable can be redirect
        if (sourceVarInfo$require || sourceVarInfo$dataType != desVarInfo$dataType || 
            sourceVarInfo$shared != desVarInfo$shared || sourceVarInfo$location != 
            desVarInfo$location || sourceVarInfo$precisionType != typeInherit(sourceVarInfo$precisionType, 
            desVarInfo$precisionType)) 
            return()
        if (desVarInfo$redirect == "NA") {
            sourceVarInfo$redirect = desVar
        } else {
            sourceVarInfo$redirect = desVarInfo$redirect
        }
        return(sourceVarInfo)
    } else {
        sourceVarInfo$redirect = desVar
        return(sourceVarInfo)
    }
}
