
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
    message("unnecessary parenthesis has been found: ",deparse(Exp))
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
    value = paste0(addParenthesis(left_res$value), op, addParenthesis(right_res$value))
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

#Exp=quote(sweep(B + 1, 2, D, `*`))
C_element_sweep<-function(varInfo, Exp, sub, opt, extCode){
  args=matchFunArg(sweep,Exp)
  margin=args$MARGIN
  x=args$x
  stats=args$STATS
  fun=as.symbol(args$FUN)
  if(margin==1){
    stats=substitute(stats[,1],list(stats=stats))
  }else{
    stats=substitute(stats[1,],list(stats=stats))
  }
  argNames=formalArgs(sweep)
  args_addition=args[!names(args)%in% argNames]
  
  newExp=substitute(fun(x,y),list(fun=fun,x=x,y=stats))
  for(i in seq_along(args_addition)){
    newExp[[names(args_addition)[i]]]=args_addition[[i]]
  }
  
  res=C_element_getCExp(varInfo, newExp,sub=sub, extCode = extCode, opt = opt)
  
  res
}
#========================No parent operation======================




#Exp=quote({ind=rowSums(A)})[[2]]
C_row_col_summary_right<-function(varInfo, leftVar,rightVar,
                                  leftSub,rightSub,
                                  loopVar,loopNum,
                                  assignmentFunc=NULL,assignmentFuncParms=NULL,
                                  pos="endCode1"){
  leftInfo=getVarInfo(varInfo,leftVar)
  precision=getTypeCXXStr(leftInfo$precisionType)
  
  leftEle = C_element_getCExp(varInfo, leftVar, 
                              sub = leftSub, 
                              opt = as.list(loopVar))
  rightEle = C_element_getCExp(varInfo, rightVar, 
                               sub = rightSub,
                               opt = as.list(loopVar), 
                               extCode = leftEle$extCode)
  tmpVar=GPUVar$getTmpVar()
  tmpDef=paste0(precision," ",tmpVar,"=0;")
  loopBody = paste0(tmpVar,"=",tmpVar,"+", rightEle$value, ";")
  if(!is.null(assignmentFunc)){
  assignment=assignmentFunc(leftEle$value,tmpVar,assignmentFuncParms)
  }else{
    assignment=paste0(leftEle$value,"=",tmpVar,";")
  }
  extCode = finalizeExtCode(rightEle$extCode)
  

  if(pos=="endCode1"){
  code = C_matrix_assignment(loopBody, 
                             loopInd1 = loopVar[1], loopEnd1 = loopNum[1],
                             loopInd2 = loopVar[2], loopEnd2 = loopNum[2], 
                             loopCode0 = extCode$L0, loopCode1 = c(tmpDef,extCode$L1), loopCode2 = extCode$L2,
                             endCode1 = assignment)
  return(code)
  }
  if(pos=="endCode0"){
    code = C_matrix_assignment(loopBody, 
                               loopInd1 = loopVar[1], loopEnd1 = loopNum[1],
                               loopInd2 = loopVar[2], loopEnd2 = loopNum[2], 
                               loopCode0 = c(tmpDef,extCode$L0), loopCode1 = extCode$L1, loopCode2 = extCode$L2,
                               endCode0 = assignment)
    return(code)
  }
  stop("Unmatched endcode!:",pos)
}

#Exp=quote({ind=sum(A)})[[2]]
C_sum_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  size_right_1 = R_nrow(varInfo, rightVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                               leftSub =c("0","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                               loopVar=c("gpu_sum_j", "gpu_sum_i"),loopNum=c(size_right_2,size_right_1),
                               pos="endCode0")
  
  code
}



#Exp=quote({ind=rowSums(A)})[[2]]
C_rowSums_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  size_left_1 = R_nrow(varInfo, leftVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                          leftSub =c("gpu_sum_i","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                          loopVar=c("gpu_sum_i", "gpu_sum_j"),loopNum=c(size_left_1,size_right_2))
  
  code
}

#Exp=quote({ind=colSums(A)})[[2]]
C_colSums_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  size_right_1 = R_nrow(varInfo, rightVar)
  size_left_2 = R_nrow(varInfo, leftVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                               leftSub =c("gpu_sum_j","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                               loopVar=c("gpu_sum_j", "gpu_sum_i"),loopNum=c(size_left_2,size_right_1)
                               )
  
  code
}


C_rowMeans_right<-function(varInfo, Exp){
  assign_rowMeans<-function(x,y,parms){
    paste0(x,"=",y,"/",parms,";")
  }
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  size_left_1 = R_nrow(varInfo, leftVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                               leftSub =c("gpu_sum_i","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                               loopVar=c("gpu_sum_i", "gpu_sum_j"),loopNum=c(size_left_1,size_right_2),
                               assignmentFunc=assign_rowMeans,assignmentFuncParms=size_right_2
                               )
  
  code
}

C_colMeans_right<-function(varInfo, Exp){
  assign_colSums<-function(x,y,parms){
    paste0(x,"=",y,"/",parms,";")
  }
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightVar=rightExp[[2]]
  size_right_1 = R_nrow(varInfo, rightVar)
  size_left_2 = R_nrow(varInfo, leftVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                               leftSub =c("gpu_sum_j","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                               loopVar=c("gpu_sum_j", "gpu_sum_i"),loopNum=c(size_left_2,size_right_1),
                               assignmentFunc=assign_colSums,assignmentFuncParms=size_right_1
  )
  
  code
}

