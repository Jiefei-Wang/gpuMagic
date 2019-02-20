
# ===========================profiler 1========================

# Profile a parameter and give the profile table back
profileVar <- function(parms, parmsWithValue,parmsConst) {
    varInfo = getEmpVarInfoTbl()
    varInfo$parmsTblName = "parms"
    varInfo$requiredVar = c()
    
    varName = names(parms)
    for (i in seq_len(length(parms))) {
        if (is(parms[[i]],"gpuMatrix")) {
            curPrecision = .type(parms[[i]])
            curDim = dim(parms[[i]])
        } else {
            curPrecision = GPUVar$default_float
            curDim = dim(as.matrix(parms[[i]]))
        }
        info = getEmpyTable()
        info$var = varName[i]
        
        
        info$precisionType = curPrecision
        info$shared = TRUE
        info$constVal = varName[i] %in% parmsConst
        info$require = TRUE
        info$initialization = FALSE
        
        
        if (varName[i] %in% parmsWithValue) {
            info$value = paste0("(", varInfo$parmsTblName, "[[", i, "]])")
        }
        info$dataType = T_matrix
        if (curDim[1] == 1 && curDim[2] == 1 && varName[i] != GPUVar$gpu_loop_data) {
            info$size1 = 1
            info$size2 = 1
        } else {
            if (varName[i] == GPUVar$gpu_loop_data) {
                info$size1 = paste0("length(", varInfo$parmsTblName, "[[", 
                  i, "]])")
                info$size2 = 1
            } else {
                info$size1 = paste0("nrow(", varInfo$parmsTblName, "[[", 
                  i, "]])")
                info$size2 = paste0("ncol(", varInfo$parmsTblName, "[[", 
                  i, "]])")
            }
        }
        
        
        varInfo = addVarInfo(varInfo, info)
        varInfo$requiredVar = c(varInfo$requiredVar, info$var)
    }
    varInfo
}

# ==================================Profiler
# 2==========================

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
        if (is.call(funArg[[i]])) 
            funArg[[i]] = eval(funArg[[i]])
    }
    return(funArg)
}

# Get the right expression profile Return value would be a list with
# the following elements: 
# ExpInfo:the expression info Exp: the
# expression errorCheck: The error check item 
# extCode: the extra code
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



is.preservedFunc <- function(func) {
    func = as.character(func)
    length(grep(GPUVar$preservedFuncPrefix, func, fixed = TRUE)) != 0
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

# This function simplify the R code and make it ready to put in the
# varInfo table
Simplify2 <- function(Exp,parentheses=TRUE) {
    res = Simplify(Exp)
    # remove the space res=trimws(gsub(', ',',',res,fixed = TRUE)) If the
    # result is a vector if(length(grep(' ',res,fixed = TRUE))!=0){
    # res=paste0('c(',gsub(' +',',',res),')') return(res) }
    if (isNumeric(res)) 
        return(res)
    res=parse(text=res)[[1]]
    res=deparse(Simplify_plus(res))
    if(parentheses){
      return(paste0("(", res, ")"))
    }else{
      return(res)
    }
}



extractVarIfFuncIsSame<-function(Exp,func){
  if(!is.call(Exp))
    Exp
  if(Exp[[1]]=="(")
    return(extractVarIfFuncIsSame(Exp[[2]],func))
  if(Exp[[1]]!=func)
    return(Exp)
  res=list()
  for(i in seq_len(length(Exp)-1)+1){
    res=c(res,extractVarIfFuncIsSame(Exp[[i]],func))
  }
  return(res)
}
#Exp=quote(max(nrow(parms[[3]]), nrow(parms[[2]])))
#Exp=quote(max((max(max(nrow(parms[[3]]), nrow(parms[[2]])), nrow(parms[[3]]))),nrow(parms[[2]])))
#Exp=quote(max(nrow(parms[[3]]), nrow(parms[[2]])))
Simplify_plus<-function(Exp){
  if(is.call(Exp)){
    func=Exp[[1]]
    Exp1=Simplify_plus(Exp[[2]])
    Exp1_char=deparse(Exp1)
    if(length(Exp)>2){
      Exp2=Simplify_plus(Exp[[3]])
      Exp2_char=deparse(Exp2)
      }
    if(func=="max"||func=="min"){
      if(Exp1_char==Exp2_char)
        return(Exp1)
      if(Exp1_char=="1"&&is.call(Exp2)&&deparse(Exp2[[1]])%in%c("length","nrow","ncol")){
        if(func=="max")
          return(Exp2)
        else
          return(1)
      }
      if(Exp2_char=="1"&&is.call(Exp1)&&deparse(Exp1[[1]])%in%c("length","nrow","ncol")){
        if(func=="max")
          return(Exp1)
        else
          return(1)
      }
      symbolList=extractVarIfFuncIsSame(Exp,func)
      if(length(symbolList)>2){
        symbolList_new=unique(symbolList)
        if(length(symbolList_new)!=length(symbolList)){
          code=paste0(deparse(func),"(",deparse(symbolList_new[[1]]),",",deparse(symbolList_new[[2]]),")")
          for(i in seq_len(length(symbolList_new)-2)+2){
            code=paste0(deparse(func),"(",code,",",deparse(symbolList_new[[i]]),")")
          }
          return(parse(text=code)[[1]])
        }else{
          return(Exp)
        }
      }
      
      
    }
    if(func=="||"){
      if(Exp1==TRUE||Exp2==TRUE)
        return(quote(TRUE))
      if(Exp1==FALSE)
        return(Exp2)
      if(Exp2==FALSE)
        return(Exp1)
    }
    if(func=="&&"){
      if(Exp1==FALSE||Exp2==FALSE)
        return(quote(FALSE))
      if(Exp1==TRUE)
        return(Exp2)
      if(Exp2==TRUE)
        return(Exp1)
    }
    if(func=="=="){
      if(Exp1_char==Exp2_char)
        return(TRUE)
    }
    if(func=="!"){
      if(is.logical(Exp1)){
        return(!Exp1)
      }
    }
    if(func=="("){
      if(is.call(Exp1)){
        funcGroup=getGroup(deparse(Exp1[[1]]))
        mathOP=length(funcGroup)!=0&&funcGroup[[1]]=="Arith"
        }else{
        mathOP=FALSE
        }
      if(length(Exp1)==1||!mathOP){
        return(Exp1)
      }
    }
    if(deparse(func)%in%c("!=",">","<")){
      if(Exp1_char==Exp2_char)
        return(FALSE)
    }
    for(i in seq_len(length(Exp)-1)+1){
      Exp[[i]]=Simplify_plus(Exp[[i]])
    }
    return(Exp)
  }
  
  return(Exp)
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
