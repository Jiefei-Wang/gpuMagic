# ==================parser 1====================
addvariableSizeInfo <- function(sizeInfo, curVarInfo) {
  curSizeinfo = data.frame(
    var = curVarInfo$var, precisionType = curVarInfo$precisionType, 
    totalSize = Simplify(paste0("(", curVarInfo$totalSize, ")*", getTypeSize(curVarInfo$precisionType))), 
    size1 = curVarInfo$size1, size2 = curVarInfo$size2, stringsAsFactors = FALSE)
  
  
    if (curVarInfo$redirect != "NA") {
        curSizeinfo$totalSize = 0
    }
    sizeInfo = rbind(sizeInfo, curSizeinfo)
    return(sizeInfo)
}
addVariableDeclaration <- function(varInfo,curVarInfo, data, offset, offsetInd,prefix) {
  CXXtype = getTypeCXXStr(curVarInfo)
  if (curVarInfo$isRef) {
      return(list(code=NULL,Info=curVarInfo))
    }
  
  
  location = paste0(prefix, " ")
  
  # If the variable is a sequence
  if (curVarInfo$isSeq) {
    if (prefix == "private") {
      curCode = paste0(CXXtype, 3, " ", curVarInfo$var, ";")
      curVarInfo$isPointer=FALSE
    } else {
      curCode = paste0(location, CXXtype, 3, "* ", curVarInfo$var, 
                       "=", "(", location, CXXtype, 3, "*)(", data, "+", offset, 
                       "[", offsetInd, "]);")
      curVarInfo$isPointer=TRUE
    }
    curVarInfo$address = curVarInfo$var
    return(list(code=curCode,Info=curVarInfo))
  }
  if (curVarInfo$redirect == "NA") {
    designSize1=curVarInfo$designSize1
    designSize2=curVarInfo$designSize2
    if (prefix == "private") {
      if(isNumeric(designSize1)&&isNumeric(designSize2)){
        size=Simplify(paste0(designSize1,"*",designSize2))
        curCode = paste0(location, CXXtype, " ", curVarInfo$var,"[",size,"];")
        curVarInfo$isPointer=TRUE
      }else{
        stop("Dynamically allocate private memory is not allowed:", curVarInfo$var)
      }
    }else{
      if(designSize1=="1"&&designSize2=="1"&&curVarInfo$shared==FALSE){
        curCode = paste0(location, CXXtype, " ", curVarInfo$var, "=", 
                         "*((", location, CXXtype, "*)(", data, "+", offset, "[", offsetInd, 
                         "]));")
        curVarInfo$isPointer=FALSE
      }else{
        curCode = paste0(location, CXXtype, "* ", curVarInfo$var, "=", 
                         "(", location, CXXtype, "*)(", data, "+", offset, "[", offsetInd, 
                         "]);")
        curVarInfo$isPointer=TRUE
      }
    }
  } else {
    curCode = paste0("#define ", curVarInfo$var, " ", curVarInfo$redirect)
    if(curVarInfo$redirect==GPUVar$return_variable){
      curVarInfo$isPointer=TRUE
    }else{
    parentInfo=getVarInfo(varInfo,curVarInfo$redirect)
    curVarInfo$isPointer=parentInfo$isPointer
    }
  }
  curVarInfo$address = curVarInfo$var
  return(list(code=curCode,Info=curVarInfo))
}


# ==================parser 2====================
C_to_R <- function(code) {
    gsub("\\((float|double|uint|int|long|ulong)\\)", "gpu_cast_\\1", code)
}
R_to_C <- function(code) {
    gsub("gpu_cast_(float|double|uint|int|long|ulong)", "\\(\\1\\)", code)
}
addParenthesis=function(x){
  if(!isSymbol(x)&&!isNumeric(x)){
    x=paste0("(",x,")")
  }
  return(x)
}
CSimplify <- function(Exp, C = TRUE,parenthesis=FALSE) {
  if(is.numeric(Exp)||is.language(Exp))
    isExp=TRUE
  else
    isExp=FALSE
    code = toCharacter(Exp)
    
    if (code == "") 
        return(code)
    if (C) 
        code = C_to_R(code)
    code = Simplify(code)
    if (C) 
        code = R_to_C(code)
    if(parenthesis){
      code=addParenthesis(code)
    }
    if(isExp)
      return(parse(text=code)[[1]])
    else
      return(code)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @param y Internal usage only
#' @rdname internalFunctions
#' @examples 
#' gpu_cast_float(10)
#' #Just to make biocCheck happy with that.
#' @return A double type data
#' @export
gpu_cast_float <- function(x) {
    as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_double <- function(x) {
    as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_uint <- function(x) {
    trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_int <- function(x) {
    trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_long <- function(x) {
    trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_ulong <- function(x) {
    trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
isgreater<-function(x,y){
  if(isNumeric(x)&&isNumeric(y))
    return(as.integer(x>y))
  else
    return(FALSE)
}
# Check if x is a valid symbol(no `` around the x)
isSymbol <- function(x) {
    x = as.symbol(x)
    length(grep("`", capture.output(x))) == 0
}
#Check if the variable x can be treated as a single value
#It means no operation can has higher priority than the opration inside x
isSingleValue<-function(x){
  if(isSymbol(x))
    return(TRUE)
  if(isNumeric(x))
    return(TRUE)
  x=toExpression(C_to_R(x))
  if(is.call(x)){
    op=deparse(x[[1]])
    if(op%in%c("[","("))
      return(TRUE)
  }
  return(FALSE)
}


getSeqAddress <- function(varInfo, var,C_symbol=FALSE) {
    curInfo = getVarInfo(varInfo, var)
    ad = curInfo$address
    if (curInfo$isPointer) {
        ad = paste0("(*", ad, ")")
    }
    from = paste0(ad, ".s0")
    to = paste0(ad, ".s1")
    by = paste0(ad, ".s2")
    length = R_getVarSize1(varInfo,var,C_symbol=C_symbol)
    data.frame(from = from, to = to, by = by, length = length, stringsAsFactors = FALSE)
}




# A general function to assign a scalar to another scalar
# The funcName shoud be the only function that is allowed to call in the scalar
# assignment
# Exp: The assignment expression
# funcName: The name of the function that is expected in the expression
# func: The function that will be called to process the function in the expression
# Exp=parse(text='A=ncol(A)')[[1]]
# funcName='ncol' 
# func=R_ncol
C_general_scalar_assignment <- function(varInfo, Exp, funcName, func) {
    leftExp = Exp[[2]]
    rightExp = Exp[[3]]
    extCode = NULL
    if (!is.list(func)) {
        func = list(func)
    }
    
    # if the left expression is the length function
    if (is.call(leftExp)) {
        curFunc = deparse(leftExp[[1]])
        if (curFunc %in% funcName) {
            value_left = func[[which(funcName == curFunc)]](varInfo, leftExp)
        } else {
            stop("Unexpected function:", deparse(Exp))
        }
    } else {
        res = R_expression_sub(varInfo, leftExp, sub = 1, sub_C = TRUE, 
            extCode = extCode)
        value_left = res$value
        extCode = res$extCode
    }
    
    
    # if the right expression is the length function
    if (is.call(rightExp)) {
        curFunc = deparse(rightExp[[1]])
        if (curFunc %in% funcName) {
            value_right = func[[which(funcName == curFunc)]](varInfo, rightExp)
        } else {
            stop("Unexpected function:", deparse(Exp))
        }
    } else {
        res = R_expression_sub(varInfo, rightExp, sub = 1, sub_C = TRUE, 
            extCode = extCode)
        value_right = res$value
        extCode = res$extCode
    }
    
    extCode = finalizeExtCode(extCode)
    code = paste0(value_left, "=", value_right, ";")
    
    return(c(unlist(extCode), code))
}
# This function is for the general matrix assignment
# The left and right variable should be able to be directly processed 
# by the oneIndex_sub function at the final stage, a func will be called with two
# parameters: value_left and value_right to do some special treatment
# for each element
C_general_matrix_assignment <- function(varInfo, leftVar, rightVar, func = matrix_assignment_func_doNothing, 
    rightBound = NULL) {
    leftDataType = getVarProperty(varInfo, leftVar, "dataType")
    if (leftDataType == T_scale) {
        sub = c(0, 0)
        code_left = C_element_getCExp(varInfo, leftVar, sub = sub)
        code_right = C_element_getCExp(varInfo, rightVar, sub = sub, extCode = code_left$extCode)
        extCode = finalizeExtCode(code_right$extCode)
        code = c(unlist(extCode), paste0(func(code_left$value, code_right$value), 
            ";"))
        
    } else {
        # dispatch accoding to if the right matrix has boundary if the right
        # matrix is a number, boundary will be ignored
        if (is.null(rightBound)) {
            
            i = "gpu_general_index_i"
            j = "gpu_general_index_j"
            sub = c(i, j)
            code_left = C_element_getCExp(varInfo, leftVar, sub = sub, 
                opt = list(j, i))
            code_right = C_element_getCExp(varInfo, rightVar, sub = sub, 
                opt = list(j, i), extCode = code_left$extCode)
            bodyCode = paste0(func(code_left$value, code_right$value), 
                ";")
            extCode = finalizeExtCode(code_right$extCode)
            
            code = C_matrix_assignment(bodyCode, loopInd1 = j, loopEnd1 = R_ncol(varInfo, 
                leftVar), loopInd2 = i, loopEnd2 = R_nrow(varInfo, leftVar), 
                loopCode0 = extCode$L0, loopCode1 = extCode$L1, loopCode2 = extCode$L2)
            
        } else {
            code_left = C_element_getCExp(varInfo, leftVar, sub = "gpu_general_index_i", 
                opt = list(c("gpu_general_index_i", "gpu_general_index_k")))
            code_right = C_element_getCExp(varInfo, rightVar, sub = "gpu_general_index_k", 
                extCode = code_left$extCode, opt = list(c("gpu_general_index_i", 
                  "gpu_general_index_k")))
            
            bodyCode = paste0(func(code_left$value, code_right$value), 
                ";")
            extCode = finalizeExtCode(code_right$extCode)
            if (!isNumeric(rightVar)) {
                defineCode = c("{", paste0(GPUVar$default_index_type, " gpu_general_index_k=0;"), 
                  paste0(GPUVar$default_index_type, " gpu_right_matrix_length=", 
                    rightBound, ";"))
                endCode = c("gpu_general_index_k=gpu_general_index_k+1;", 
                  paste0("if(gpu_general_index_k==gpu_right_matrix_length){"), 
                  "gpu_general_index_k=0;", "}")
            } else {
                defineCode = NULL
                endCode = NULL
            }
            code = c(defineCode, C_matrix_assignment(bodyCode, loopInd1 = "gpu_general_index_i", 
                loopEnd1 = R_length(varInfo, leftVar), loopCode0 = extCode$L0, 
                loopCode1 = extCode$L1, endCode1 = endCode))
        }
    }
    
    code
}

# This function will not check the variable in the varInfo 
#it just create a for loop in C code format 
# for(loopStart1:loopEnd1-1){
# loopCode1 
# for(loopStart2:loopEnd2-1){ 
# loopCode2 
# bodyCode 
# endCode2 
# }
# endCode1 
# }
C_matrix_assignment <- function(bodyCode, loopInd1, loopStart1 = "0", loopEnd1, 
    loopInd2 = NULL, loopStart2 = "0", loopEnd2 = NULL, loopCode0 = NULL, 
    loopCode1 = NULL, loopCode2 = NULL, endCode1 = NULL, endCode2 = NULL) {
  loopLen1=CSimplify(paste0(loopEnd1,"-",loopStart1))
  loopLen2=paste0(loopEnd2,"-",loopStart2)
  
  if(loopLen1=="0")
    return(NULL)
  if(!is.null(loopEnd2)){
    loopLen2=CSimplify(loopLen2)
    if(loopLen2=="0")
      return(NULL)
  }
  if(loopLen1=="1"){
    if(isSymbol(loopStart1)||isNumeric(loopStart1)){
    loop1Def=paste0("#define ",loopInd1," ",loopStart1)
    }else{
      loop1Def=paste0("#define ",loopInd1," (",loopStart1,")")
    }
    loop1Def=c("{",loop1Def)
    loopCode1=gsub("break;","//break;",loopCode1,fixed=TRUE)
    endCode1=gsub("break;","//break;",endCode1,fixed=TRUE)
    endCode1=c(endCode1,paste0("#undef ",loopInd1))
  }else{
    loop1Def=paste0(
      "for(", GPUVar$default_index_type, " ", loopInd1, 
      "=", loopStart1, ";", loopInd1, "<", loopEnd1, ";", loopInd1, "++){")
  }
  if(loopLen2=="1"){
    if(isSymbol(loopStart2)||isNumeric(loopStart2)){
      loop2Def=paste0("#define ",loopInd2," ",loopStart2)
    }else{
      loop2Def=paste0("#define ",loopInd2," (",loopStart2,")")
    }
    loop2Def=c("{",loop2Def)
    loopCode2=gsub("break;","//break;",loopCode2,fixed=TRUE)
    endCode2=gsub("break;","//break;",endCode2,fixed=TRUE)
    endCode2=c(endCode2,paste0("#undef ",loopInd2))
  }else{
  loop2Def=paste0(
    "for(", GPUVar$default_index_type, " ", loopInd2, "=", 
                  loopStart2, ";", loopInd2, "<", loopEnd2, ";", loopInd2,  "++){")
  }
  if (!is.null(loopInd2)) {
    loopCode2 = c(loop2Def, loopCode2)
    endCode2 = c(endCode2, "}")
  }
  code = c(loopCode0, loop1Def,loopCode1,loopCode2, bodyCode, endCode2, endCode1, "}")
  code
}

matrix_assignment_func_doNothing <- function(value_left, value_right) {
    paste0(value_left, "=", value_right)
}


