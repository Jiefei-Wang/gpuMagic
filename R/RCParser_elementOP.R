
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
  return(right_res)
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
      res_twoIndex=one_to_two_index(sub,extCode = sub1$extCode,rowNum=rowNum,colNum=colNum)
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
C_sum_mean_right<-function(varInfo, Exp){
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  rightFunc=rightExp[[1]]
  rightVar=rightExp[[2]]
  assign_func<-function(x,y,parms){
    if(parms==1)
      return(paste0(x,"=",y,";"))
    else{
      return(paste0(x,"=(",GPUVar$default_float,")",addParenthesis(y),"/",addParenthesis(parms),";"))
    }
  }
  if(rightFunc=="sum"){
    parms=1
  }else{
    parms=R_length(varInfo,rightVar)
  }
  size_right_1 = R_nrow(varInfo, rightVar)
  size_right_2 = R_ncol(varInfo, rightVar)
  code=C_row_col_summary_right(varInfo, leftVar,rightVar,
                               leftSub =c("0","0"),rightSub =c("gpu_sum_i", "gpu_sum_j"),
                               loopVar=c("gpu_sum_j", "gpu_sum_i"),loopNum=c(size_right_2,size_right_1),
                               assignmentFunc=assign_func,assignmentFuncParms=parms,
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

#Exp=quote({B=sort(A)})[[2]]
C_ascending_sort_right<-function(varInfo,Exp){
  sort_macro<-function(dataPrecision,data,attachedInd,index,from,to){
    if(from==0){
      len=to
    }else{
      len=paste0(to,"-",from)
    }
    if(is.null(attachedInd)){
      attachedCode=NULL
    }else{
      attachedCode=c(
        paste0(GPUVar$default_index_type," gpu_sortMacro_attachTmp=",attachedInd,";"),
        paste0(attachedInd,"=",replaceIndex(attachedInd,index,"gpu_sortMacro_curInd"),";"),
        paste0(replaceIndex(attachedInd,index,"gpu_sortMacro_curInd"),"=gpu_sortMacro_attachTmp;"))
    }
    code=c(
      paste0("for(default_index_type gpu_sortMacro_i=1;gpu_sortMacro_i<",len,";gpu_sortMacro_i=gpu_sortMacro_i+1){"),
      paste0("default_index_type ",index,";"),
      paste0(dataPrecision," gpu_sortMacro_target;"),
      paste0("default_index_type gpu_sortMacro_curInd=",from,";"),
      paste0(dataPrecision," gpu_sortMacro_curvalue=",replaceIndex(data,index,from),";"),
      paste0("for(",index,"=",from,"+1;",index,"<",to,"-gpu_sortMacro_i;",index,"=",index,"+1){"),
      paste0("gpu_sortMacro_target=",data,";"),
      "if(gpu_sortMacro_target>gpu_sortMacro_curvalue){",
      "gpu_sortMacro_curvalue=gpu_sortMacro_target;",
      paste0("gpu_sortMacro_curInd=",index,";"),
      "}",
      "}",
      paste0(index,"=",to,"-gpu_sortMacro_i;"),
      paste0("gpu_sortMacro_target=",data,";"),
      "if(gpu_sortMacro_target<gpu_sortMacro_curvalue){",
      paste0(replaceIndex(data,index,"gpu_sortMacro_curInd"),"=gpu_sortMacro_target;"),
      paste0(data,"=gpu_sortMacro_curvalue;"),
      attachedCode,
      "}",
      "}"
    )
    code
  }
  
  
  replaceIndex<-function(data,index,newInd){
    gsub(index,newInd,data,fixed = TRUE)
  }
  
  leftVar = Exp[[2]]
  rightExp = Exp[[3]]
  args=matchFunArg(sort,rightExp)
  rightVar=args$x
  leftInfo=getExpInfo(varInfo,leftVar)$ExpInfo
  
  
  private_vector_size = GPUVar$private_vector_size
  defaultPrecision = leftInfo$precisionType
  #privateVecLength = private_vector_size/getTypeSize(defaultPrecision)/2
  gpu_private_length=12
  
  
  macroDef = c(
    paste0("gpu_sort_vec_len ", addParenthesis(R_length(varInfo, leftVar))), 
    paste0("gpu_private_length ",gpu_private_length)
  )
  
  macroDef = paste0("#define ", macroDef)
  macroUndef = c("gpu_sort_vec_len","gpu_private_length")
  macroUndef = paste0("#undef ", macroUndef)
  supportVarDef=c(
  paste0(defaultPrecision, " gpu_private_space[gpu_private_length];"),
  paste0("default_index_type gpu_private_space_ind[gpu_private_length];")
  )
  
  #Allocate the values
  leftEle = C_element_getCExp(varInfo, leftVar, 
                              sub = "gpu_sort_k", 
                              opt = "gpu_sort_k")
  
  rightEle = C_element_getCExp(varInfo, rightVar, 
                              sub = "gpu_sort_k", 
                              opt = "gpu_sort_k",
                              extCode = leftEle$extCode)
  
  extCode = finalizeExtCode(rightEle$extCode)
  assignValue=c(
    extCode$L0,
    "for(default_index_type gpu_sort_k=0;gpu_sort_k<gpu_sort_vec_len;gpu_sort_k++){",
    extCode$L1,
    paste0(leftEle$value,"=",rightEle$value,";"),
    "}"
  )
  
  
  #Read a part of data info the private space
  #full read
  
  extCode = finalizeExtCode(leftEle$extCode)
  write_to_private_full=c(
    extCode$L0,
    "for(default_index_type gpu_sort_k=0;gpu_sort_k<gpu_private_length;gpu_sort_k++){",
    extCode$L1,
    paste0("gpu_private_space[gpu_sort_k]=",leftEle$value,";"),
    "gpu_private_space_ind[gpu_sort_k]=gpu_sort_k;",
    "}",
    "//Sort the private space",
    sort_macro(defaultPrecision,"gpu_private_space[gpu_sort_k]","gpu_private_space_ind[gpu_sort_k]",
               "gpu_sort_k",0,"gpu_private_length")
  )
  
  write_to_private_partial=c(
    "//Sort the last part of the values",
    extCode$L0,
    "gpu_sort_length=gpu_sort_vec_len-gpu_private_length*(gpu_sort_length-1);",
    "for(default_index_type gpu_sort_k=0;gpu_sort_k<gpu_sort_length;gpu_sort_k++){",
    extCode$L1,
    paste0("gpu_private_space[gpu_sort_k]=",leftEle$value,";"),
    "}",
    "//Sort the private space",
    sort_macro(defaultPrecision,"gpu_private_space[gpu_sort_k]",NULL,
               "gpu_sort_k",0,"gpu_sort_length"),
    "//Write back the result",
    "for(default_index_type gpu_sort_k=0;gpu_sort_k<gpu_sort_length;gpu_sort_k++){",
    extCode$L1,
    paste0(leftEle$value,"=gpu_private_space[gpu_sort_k];"),
    "}"
  )
  
  
  curRightValue=C_element_getCExp(varInfo, leftVar, 
                                  sub = "gpu_sort_j", 
                                  opt = "gpu_sort_j")
  curRightValue_extCode=finalizeExtCode(curRightValue$extCode)
  endValue=C_element_getCExp(varInfo, leftVar, 
                                  sub = "gpu_sort_j", 
                                  opt = NULL)
  endOriginValue=C_element_getCExp(varInfo, leftVar, 
                                        sub = "gpu_private_space_ind[gpu_sort_t]", 
                                        opt = NULL,extCode = endValue$extCode)
  end_extCode=finalizeExtCode(endOriginValue$extCode)
  
  
  code=c(
    "{",
    macroDef,
    supportVarDef,
    assignValue,
    paste0("default_index_type gpu_sort_length=ceil((default_float)gpu_sort_vec_len/gpu_private_length);"),
    "for(default_index_type gpu_sort_i=0;gpu_sort_i<gpu_sort_length-1;gpu_sort_i++){",
    write_to_private_full,
    paste0(defaultPrecision," gpu_sort_curValue;"),
    curRightValue_extCode$L0,
    "for(default_index_type gpu_sort_j=gpu_private_length;gpu_sort_j<gpu_sort_vec_len-gpu_private_length*gpu_sort_i;
    gpu_sort_j++){",
    curRightValue_extCode$L1,
    paste0("gpu_sort_curValue=",curRightValue$value,";"),
    "if(gpu_sort_curValue>gpu_private_space[0]){",
    "for(default_index_type gpu_sort_t=0;gpu_sort_t<gpu_private_length;gpu_sort_t++){",
    "if(gpu_sort_curValue>gpu_private_space[gpu_sort_t]){",
    "if(gpu_sort_t==gpu_private_length-1||gpu_sort_curValue<=gpu_private_space[gpu_sort_t+1]){",
    "for(default_index_type gpu_sort_t2=0;gpu_sort_t2<gpu_sort_t;gpu_sort_t2++){",
    "gpu_private_space[gpu_sort_t2]=gpu_private_space[gpu_sort_t2+1];",
    "gpu_private_space_ind[gpu_sort_t2]=gpu_private_space_ind[gpu_sort_t2+1];",
    "}",
    "gpu_private_space[gpu_sort_t]=gpu_sort_curValue;",
    "gpu_private_space_ind[gpu_sort_t]=gpu_sort_j;",
    "break;",
    "}",
    "}",
    "}",
    "}",
    "}",
    "//When the boat arrive to the end, release all the values",
    "for(default_index_type gpu_sort_last=0;gpu_sort_last<gpu_private_length;gpu_sort_last++){",
    "default_index_type gpu_sort_j=gpu_sort_last+gpu_sort_vec_len-gpu_private_length*(gpu_sort_i+1);",
    curRightValue_extCode$L1,
    paste0(defaultPrecision," gpu_sort_curValue=",curRightValue$value,";"),
    "//Move the non-interested value to a safe place",
    end_extCode$L0,
    "if(gpu_sort_curValue<gpu_private_space[0]){",
    "for(default_index_type gpu_sort_t=0;gpu_sort_t<gpu_private_length;gpu_sort_t++){",
    "if(gpu_private_space_ind[gpu_sort_t]<gpu_sort_vec_len-gpu_private_length*(gpu_sort_i+1)){",
    paste0(endOriginValue$value,"=gpu_sort_curValue;"),
    paste0(endValue$value,"=gpu_private_space[gpu_sort_last];"),
    "gpu_private_space_ind[gpu_sort_t]=gpu_sort_j;",
    "break;",
    "}",
    "}",
    "}else{",
    paste0(endValue$value,"=gpu_private_space[gpu_sort_last];"),
    "}",
    "}",
    "}",
    write_to_private_partial,
    macroUndef,
    "}"
  )
  code
}
# sort_macro<-function(data,index,from,to){
#   len=to-from+1
#   for(i in 1:(len-1)){
#     curind=from
#     curValue=data[curind]
#     for(index in (from+1):(to-i)){
#       if(data[index]>curValue){
#         curValue=data[index]
#         curind=index
#       }
#     }
#     if(data[to-i+1]<curValue){
#       data[curind]=data[to-i+1]
#       data[to-i+1]=curValue
#     }
#     data
#   }
#   
#   data
# }


  
  
  
  
  
  

