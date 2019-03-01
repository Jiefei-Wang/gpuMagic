
#Exp=quote({a1 = A[ind2, ]})[[2]]
C_element_OP <- function(varInfo, Exp) {
  leftExp = Exp[[2]]
  rightExp = Exp[[3]]
  
  rowNum=R_nrow(varInfo,leftExp)
  colNum=R_ncol(varInfo,leftExp)
  if(rowNum==1&&colNum==1){
    dataType=T_scale
  }else{
    dataType=T_matrix
  }
  if (dataType == T_scale) {
    sub = c("0", "0")
    leftElement = C_element_getCExp(varInfo, leftExp, sub = sub, opt = NULL)
    rightElement = C_element_getCExp(varInfo, rightExp, sub = sub, extCode = leftElement$extCode, opt = NULL)
  } else {
    sub = c("gpu_element_i", "gpu_element_j")
    leftElement = C_element_getCExp(varInfo, leftExp, sub = sub, opt = list("gpu_element_j", "gpu_element_i"))
    rightElement = C_element_getCExp(varInfo, rightExp, sub = sub, extCode = leftElement$extCode, opt = list("gpu_element_j", 
                                                                                                             "gpu_element_i"))
  }
  
  extCode = finalizeExtCode(rightElement$extCode)
  
  assignmentCode = paste0(leftElement$value, "=", rightElement$value, ";")
  if (dataType == T_scale) {
    extCode = unlist(extCode)
    if (is.null(extCode)) {
      code = assignmentCode 
    }else {
      code = c("{", extCode, assignmentCode, "}")
    }
  } else {
    code = C_matrix_assignment(
      assignmentCode, 
      loopInd1 = "gpu_element_j", loopEnd1 = R_ncol(varInfo, leftExp), 
      loopInd2 = "gpu_element_i",  loopEnd2 = R_nrow(varInfo, leftExp), 
      loopCode0 = NULL, loopCode1 = extCode$L1, loopCode2 = extCode$L2)
  }
  return(code)
}


# 0-based index 
# Return: 
# list: value
#       extCode
C_element_getCExp <- function(varInfo, Exp, sub, extCode = NULL, opt = NULL) {
  if (is.null(extCode)) 
    extCode = createExtCode(opt)
  
  vars=extractVars(deparse(Exp))
  numericVar=vapply(vars,isNumeric,logical(1))
  
  if (sum(!numericVar)==0) {
    Exp=Simplify(Exp)
    res = list(value = toCharacter(Exp), extCode = extCode)
    return(res)
  }
  
  if (is.symbol(Exp)) {
    res = R_expression_sub(varInfo, Exp, sub = sub, opt = opt, extCode = extCode)
    return(res)
  }
  
  
  func = paste0("<-", deparse(Exp[[1]]))
  C_func = .cFuncs[[func]]
  if (!is.null(C_func)) {
    res = C_func(varInfo, Exp, sub, opt = opt, extCode = extCode)
    return(res)
  }
  stop("Unsupported function: ", deparse(Exp))
  
}
C_element_parenthesis <- function(varInfo, Exp, sub, opt, extCode) {
  element = Exp[[2]]
  res = C_element_getCExp(varInfo, element, sub, extCode = extCode, opt = opt)
  if(!isSingleValue(res$value)){
    res$value = paste0("(", res$value, ")")
  }else{
    warning("unnecessary parenthesis has been found: ",deparse(Exp))
  }
  return(res)
}

C_element_arithmatic <- function(varInfo, Exp, sub, opt, extCode) {
  op = deparse(Exp[[1]])
  leftEle = Exp[[2]]
  rightEle = Exp[[3]]
  left_res = C_element_getCExp(varInfo, leftEle, sub, extCode = extCode, opt = opt)
  right_res = C_element_getCExp(varInfo, rightEle, sub, extCode = left_res$extCode, opt = opt)
  
  extCode = right_res$extCode
  value=NULL
  if(op%in%c("+","-","*","/",">","<",">=","<=","==","!=")){
    value = paste0(left_res$value, op, right_res$value)
    if (op == "/") 
      value = paste0("(", GPUVar$default_float, ")", value)
  }
  if(op=="^"){
    value = paste0("pow(",left_res$value,",", right_res$value,")")
  }
  if(is.null(value))
    stop("Unexpected error")
  res = list(value = value, extCode = extCode)
  return(res)
}

C_element_floor <- function(varInfo, Exp, sub, opt, extCode) {
  element = Exp[[2]]
  res = C_element_getCExp(varInfo, element, sub, extCode = extCode, opt = opt)
  res$value = paste0("floor(", res$value, ")")
  return(res)
}
C_element_ceil <- function(varInfo, Exp, sub, opt, extCode) {
  element = Exp[[2]]
  res = C_element_getCExp(varInfo, element, sub, extCode = extCode, opt = opt)
  res$value = paste0("ceil(", res$value, ")")
  return(res)
}

C_element_power<-function(varInfo, Exp, sub, opt, extCode){
  leftEle = Exp[[2]]
  rightEle = Exp[[3]]
  left_res = C_element_getCExp(varInfo, leftEle, sub, extCode = extCode, opt = opt)
  right_res = C_element_getCExp(varInfo, rightEle, sub, extCode = left_res$extCode, opt = opt)
  
}
C_element_abs<-function(varInfo, Exp, sub, opt, extCode){
  element = Exp[[2]]
  res = C_element_getCExp(varInfo, element, sub, extCode = extCode, opt = opt)
  if(Exp[[1]]=="abs_int")
    res$value = paste0("abs(", res$value, ")")
  else
    res$value = paste0("fabs(", res$value, ")")
  return(res)
}
processSub<-function(varInfo, Exp, sub, opt, extCode){
  if(Exp==""){
    res = list(value = sub, extCode = extCode)
    return(res)
  }
  res=C_element_getCExp(varInfo, Exp,sub,extCode = extCode, opt = opt)
  res$value=CSimplify(paste0(res$value,"-1"))
  return(res)
}

C_element_sub<-function(varInfo, Exp, sub, opt, extCode){
  args=matchBracketFunc(Exp)
  targetExp=Exp[[2]]
  sub1=processSub(varInfo, args$i,sub[1],extCode = extCode, opt = opt)
  
  if(length(sub) == 1){
    #two index bracket -- one index sub
    if(!is.null(args$j)){
      rowNum = R_nrow(varInfo,Exp)
      colNum=R_ncol(varInfo,Exp)
      res_twoIndex=one_to_two_index(sub1$value,extCode = sub1$extCode,rowNum=rowNum,colNum=colNum)
      sub_new=c(res_twoIndex$i,res_twoIndex$j)
      res = C_element_sub(varInfo, Exp, sub=sub_new, extCode = res_twoIndex$extCode, opt = opt)
    }else{
      #one index bracket -- one index sub
      res = C_element_getCExp(varInfo, targetExp,sub=sub1$value, extCode = sub1$extCode, opt = opt)
    }
  }else{
    if(!is.null(args$j)){
      sub2=processSub(varInfo, args$j,sub[2],extCode = sub1$extCode, opt = opt)
      #two index bracket -- two index sub
      res = C_element_getCExp(varInfo, targetExp,sub=c(sub1$value,sub2$value), extCode = sub2$extCode, opt = opt)
    }else{
      #one index bracket -- two index sub
      res = C_element_getCExp(varInfo, targetExp,sub=sub1$value, extCode = sub1$extCode, opt = opt)
    }
  }
  
  return(res)
}

# Exp=parse(text='ind=length(A)')[[1]]
C_element_length <- function(varInfo, Exp, sub, opt, extCode) {
  res=list(extCode=extCode)
  res$value=R_length(varInfo, Exp[[2]],C_symbol=FALSE)
  return(res)
}

# Exp=parse(text='A=nrow(A)')[[1]]
C_element_nrow <- function(varInfo, Exp, sub, opt, extCode) {
  res=list(extCode=extCode)
  res$value=R_nrow(varInfo, Exp[[2]],C_symbol=FALSE)
  return(res)
}
# Exp=parse(text='A=ncol(A)')[[1]]
C_element_ncol <- function(varInfo, Exp, sub, opt, extCode) {
  res=list(extCode=extCode)
  res$value=R_ncol(varInfo, Exp[[2]],C_symbol=FALSE)
  return(res)
}
#========================No parent operation======================
#Exp=quote({ind=sum(A)})[[2]]
C_sum_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  
  leftInfo=getVarInfo(varInfo,leftVar)
  precision=getTypeCXXStr(leftInfo$precisionType)
  
  leftEle = C_element_getCExp(varInfo, leftVar, 
                              sub = c("0","0"), 
                              opt = list("gpu_sum_j", "gpu_sum_i"))
  rightEle = C_element_getCExp(varInfo, rightVar, 
                               sub = c("gpu_sum_i", "gpu_sum_j"),
                               opt = list("gpu_sum_j", "gpu_sum_i"), 
                               extCode = leftEle$extCode)
  size_right_1 = R_nrow(varInfo, rightVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  tmpVar=GPUVar$getTmpVar()
  tmpDef=paste0(precision," ",tmpVar,"=0;")
  loopBody = paste0(tmpVar,"=",tmpVar,"+", rightEle$value, ";")
  extCode = finalizeExtCode(rightEle$extCode)
  
  loopCode0=c(tmpDef,
              extCode$L0)
  code = C_matrix_assignment(
    loopBody, 
    loopInd1 = "gpu_sum_j", loopEnd1 = size_right_2,
    loopInd2 = "gpu_sum_i", loopEnd2 = size_right_1, 
    loopCode0 = loopCode0, loopCode1 = extCode$L1, loopCode2 = extCode$L2,
    endCode0=paste0(leftEle$value,"=",tmpVar,";"))
  
  code
}


#Exp=quote({ind=colSums(A)})[[2]]
C_colSums_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  
  leftInfo=getVarInfo(varInfo,leftVar)
  precision=getTypeCXXStr(leftInfo$precisionType)
  
  leftEle = C_element_getCExp(varInfo, leftVar, 
                              sub = c("gpu_sum_j","0"), 
                              opt = list("gpu_sum_j", "gpu_sum_i"))
  rightEle = C_element_getCExp(varInfo, rightVar, 
                               sub = c("gpu_sum_i", "gpu_sum_j"),
                               opt = list("gpu_sum_j", "gpu_sum_i"), 
                               extCode = leftEle$extCode)
  size_right_1 = R_nrow(varInfo, rightVar)
  size_left_2 = R_nrow(varInfo, leftVar)
  tmpVar=GPUVar$getTmpVar()
  tmpDef=paste0(precision," ",tmpVar,"=0;")
  loopBody = paste0(tmpVar,"=",tmpVar,"+", rightEle$value, ";")
  assignment=paste0(leftEle$value,"=",tmpVar,";")
  extCode = finalizeExtCode(rightEle$extCode)
  
  code = C_matrix_assignment(loopBody, 
                             loopInd1 = "gpu_sum_j", loopEnd1 = size_left_2,
                             loopInd2 = "gpu_sum_i", loopEnd2 = size_right_1, 
                             loopCode0 = extCode$L0, loopCode1 = c(tmpDef,extCode$L1), loopCode2 = extCode$L2,
                             endCode1 = assignment)
  
  code
}


#Exp=quote({ind=rowSums(A)})[[2]]
C_rowSums_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  
  leftInfo=getVarInfo(varInfo,leftVar)
  precision=getTypeCXXStr(leftInfo$precisionType)
  
  leftEle = C_element_getCExp(varInfo, leftVar, 
                              sub = c("gpu_sum_i","0"), 
                              opt = list("gpu_sum_i", "gpu_sum_j"))
  rightEle = C_element_getCExp(varInfo, rightVar, 
                               sub = c("gpu_sum_i", "gpu_sum_j"),
                               opt = list("gpu_sum_i", "gpu_sum_j"), 
                               extCode = leftEle$extCode)
  size_left_1 = R_nrow(varInfo, leftVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  tmpVar=GPUVar$getTmpVar()
  tmpDef=paste0(precision," ",tmpVar,"=0;")
  loopBody = paste0(tmpVar,"=",tmpVar,"+", rightEle$value, ";")
  assignment=paste0(leftEle$value,"=",tmpVar,";")
  extCode = finalizeExtCode(rightEle$extCode)
  
  code = C_matrix_assignment(loopBody, 
                             loopInd1 = "gpu_sum_i", loopEnd1 = size_left_1,
                             loopInd2 = "gpu_sum_j", loopEnd2 = size_right_2, 
                             loopCode0 = extCode$L0, loopCode1 = c(tmpDef,extCode$L1), loopCode2 = extCode$L2,
                             endCode1 = assignment)
  code
}