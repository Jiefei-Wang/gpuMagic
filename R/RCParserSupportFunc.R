# ==================parser 1====================
getPointerType<-function(varInfo,curVar){
  if(curVar==GPUVar$return_variable){
    return(TRUE)
  }
  curInfo = getVarInfo(varInfo, curVar, 0)
  if(!is.na(curInfo$isPointer))
    return(curInfo$isPointer)
  if(curInfo$redirect != "NA"){
    return(getPointerType(varInfo,curInfo$redirect))
  }
  if(curInfo$isRef){
    refVar=deparse(parse(text=curInfo$specialContent)[[1]][[2]])
    return(getPointerType(varInfo,refVar))
  }
  location=curInfo$location
  shared=curInfo$shared
  if (curInfo$isSeq) {
    if (location=="local"&&shared==FALSE) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  if(location=="local"&&shared==FALSE&&curInfo$dataType==T_scale){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

addvariableSizeInfo <- function(sizeInfo, curInfo,sizeHeader,matrixInd) {
  curSizeinfo = data.frame(
    var = curInfo$var, precisionType = curInfo$precisionType, 
    size1 = curInfo$size1, size2 = curInfo$size2,
    size1_char=paste0(sizeHeader,"1(",matrixInd,")"),
    size2_char=paste0(sizeHeader,"2(",matrixInd,")"),
    length = curInfo$designSize,
    sizeInByte = Simplify(paste0("(", curInfo$designSize, ")*", getTypeSize(curInfo$precisionType))), 
    stringsAsFactors = FALSE)
  
  
    if (curInfo$redirect != "NA"||curInfo$require) {
        curSizeinfo$sizeInByte = 0
    }
    sizeInfo = rbind(sizeInfo, curSizeinfo)
    return(sizeInfo)
}
#find the root source variable for the redirected variable
findRedirectRoot<-function(varInfo,var){
  if(var==GPUVar$return_variable){
    return(var)
  }
  curInfo=getVarInfo(varInfo,var)
  
  if(curInfo$redirect!="NA"){
    return(findRedirectRoot(varInfo,curInfo$redirect))
  }else{
    return(curInfo$address)
  }
}

addVariableDeclaration <- function(varInfo,curInfo, data, offset, offsetInd,prefix) {
  CXXtype = getTypeCXXStr(curInfo$precisionType)
  worker_offset=GPUVar$worker_offset
  location = paste0(prefix, " ")
  curInfo$address = curInfo$var
  
  if(curInfo$redirect!="NA"){
    curCode = paste0("#define ",curInfo$var," ",findRedirectRoot(varInfo,curInfo$redirect))
    return(list(code=curCode,Info=curInfo))
  }
  
  # If the variable is a sequence
  if (curInfo$isSeq) {
    if (!curInfo$isPointer) {
      curInfo$address= paste0("gpu_seq_",curInfo$var)
      curCode = NULL
    } else {
      curCode = paste0(location, CXXtype, 3, "* ", curInfo$var, 
                       "=", "((", location, CXXtype, 3, "*)(", data, "+", offset, 
                       "(", offsetInd, ")))+",worker_offset,";")
    }
    return(list(code=curCode,Info=curInfo))
  }
  
  designSize=curInfo$designSize
  if (prefix == "private") {
    if(isNumeric(designSize)){
      curCode = paste0(location, CXXtype, " ", curInfo$var,"[",designSize,"];")
    }else{
      stop("Dynamically allocate private memory is not allowed:", curInfo$var)
    }
  }else{
      curCode = paste0(location, CXXtype, "* ", curInfo$var, "=", 
                       "((", location, CXXtype, "*)(", data, "+", offset, "(", offsetInd, 
                       ")))+",worker_offset,";")
  }
  return(list(code=curCode,Info=curInfo))
}

addVariableDeclaration_NonPointer<-function(varInfo,curInfo){
  curVar=curInfo$var
  CXXtype = getTypeCXXStr(curInfo$precisionType)
  curInfo$address=curVar
  if(curInfo$redirect!="NA"){
    redirectInfo=getVarInfo(varInfo,curInfo$redirect)
    curCode = paste0("#define ",curVar," ",redirectInfo$address)
    return(list(code=curCode,Info=curInfo))
  }
  
  if (curInfo$isSeq){
    curCode = paste0(CXXtype, 3, " ", curInfo$var, ";")
  }else{
    curCode = paste0(CXXtype, " ", curVar, ";")
  }
  return(list(code=curCode,Info=curInfo))
}


getSizeVar<-function(varName,i){
  paste0(GPUVar$matrix_size_prefix, varName,"_dim_",i)
}


# ==================parser 2====================
C_to_R <- function(code) {
    gsub("\\((float|double|uint|int|long|ulong)\\)", "gpu_cast_\\1", code)
}
R_to_C <- function(code) {
    gsub("gpu_cast_(float|double|uint|int|long|ulong)", "\\(\\1\\)", code)
}
addParenthesis=function(x){
  if(!isSingleValue(x)){
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
    if(curInfo$isPointer){
      ad = paste0("(*", ad, ")")
      from = paste0(ad, ".s0")
      to = paste0(ad, ".s1")
      by = paste0(ad, ".s2")
    }else{
      from = paste0(ad, "_from")
      to = paste0(ad, "_to")
      by = paste0(ad, "_by")
    }
    length = getSizeVar(deparse(var),1)
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
        res = C_element_getCExp(varInfo, leftExp, sub = 0, extCode = extCode)
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
        res = C_element_getCExp(varInfo, rightExp, sub = 0, extCode = extCode)
        value_right = res$value
        extCode = res$extCode
    }
    
    extCode = finalizeExtCode(extCode)
    code = paste0(value_left, "=", value_right, ";")
    
    return(c(unlist(extCode), code))
}

# This function will not check the variable in the varInfo 
#it just create a for loop in C code format 
# loopCode0
# for(loopStart1:loopEnd1-1){
# loopCode1 
# for(loopStart2:loopEnd2-1){ 
# loopCode2 
# bodyCode 
# endCode2 
# }
# endCode1 
# }
#C_matrix_assignment("body",loopInd1="i",loopEnd1="10")
#C_matrix_assignment("body",loopInd1="i",loopEnd1="10",loopCode1="loop1")
#C_matrix_assignment("body;\nbreak;",loopInd1="i",loopEnd1="1",loopCode1="loop1")
#C_matrix_assignment("body;\nbreak;",loopInd1="i",loopEnd1="1",loopInd2="j",loopEnd2="1",loopCode1="loop1",loopCode0="loop0")
C_matrix_assignment <- function(bodyCode, 
                                loopInd1, loopStart1 = "0", loopEnd1, 
                                loopInd2 = NULL, loopStart2 = "0", loopEnd2 = NULL, 
                                loopCode0 = NULL, loopCode1 = NULL, loopCode2 = NULL, 
                                endCode0=NULL, endCode1 = NULL, endCode2 = NULL,
                                autoBracket=TRUE) {
  loopLen1=CSimplify(paste0(loopEnd1,"-",loopStart1))
  if(loopLen1=="0")
    return(NULL)
  
  loopInfo1=simplifyLoop(loopInd1,loopStart1,loopEnd1,loopLen1,loopCode1,endCode1)
  
  if(is.null(loopEnd2)){
    bracketNeeded=detectBracketRequirement(loopCode0,loopInfo1$loopCode,NULL)
    bracketNeeded=bracketNeeded|c(FALSE,TRUE,FALSE)
    bracket_start=sapply(bracketNeeded,function(x)ifelse(x,"{",""))
    bracket_end=sapply(bracketNeeded,function(x)ifelse(x,"}",""))
    if(!loopInfo1$needBracket){
      bodyCode=gsub("break;","//break;",bodyCode,fixed=TRUE)
    }
    
    code = c(bracket_start[1],loopCode0, 
             loopInfo1$loopDef,bracket_start[2],loopInfo1$loopCode,
             bodyCode,
             loopInfo1$endCode,bracket_end[2],
             endCode0, bracket_end[1])
    
    
  }else{
    loopLen2=paste0(loopEnd2,"-",loopStart2)
    loopLen2=CSimplify(loopLen2)
    if(loopLen2=="0")
      return(NULL)
    
    loopInfo2=simplifyLoop(loopInd2,loopStart2,loopEnd2,loopLen2,loopCode2,endCode2)
    if(!loopInfo2$needBracket){
      bodyCode=gsub("break;","//break;",bodyCode,fixed=TRUE)
    }
    
    bracketNeeded=detectBracketRequirement(loopCode0,loopInfo1$loopCode,loopInfo2$loopCode)
    bracketNeeded=bracketNeeded|c(FALSE,loopInfo1$needBracket,TRUE)
    bracket_start=sapply(bracketNeeded,function(x)ifelse(x,"{",""))
    bracket_end=sapply(bracketNeeded,function(x)ifelse(x,"}",""))
    
    code = c(bracket_start[1],loopCode0, 
             loopInfo1$loopDef,bracket_start[2],loopInfo1$loopCode,
             loopInfo2$loopDef,bracket_start[3],loopInfo2$loopCode,
             bodyCode,
             loopInfo2$endCode,bracket_end[3],
             loopInfo1$endCode,bracket_end[2],
             endCode0,bracket_end[1])
    
  }
  empCode=which(code=="")
  if(length(empCode)!=0)
   code=code[-empCode]
  return(code)
}

simplifyLoop<-function(loopInd,loopStart,loopEnd,loopLen,loopCode,endCode){
  if(loopLen=="1"){
    if(isSymbol(loopStart)||isNumeric(loopStart)){
      loopDef=paste0("#define ",loopInd," ",loopStart)
    }else{
      loopDef=paste0("#define ",loopInd," (",loopStart,")")
    }
    loopCode=gsub("break;","//break;",loopCode,fixed=TRUE)
    endCode=gsub("break;","//break;",endCode,fixed=TRUE)
    endCode=c(endCode,paste0("#undef ",loopInd))
    needBracket=FALSE
  }else{
    loopDef=paste0(
      "for(", GPUVar$default_index_type, " ", loopInd, 
      "=", loopStart, ";", loopInd, "<", loopEnd, ";", loopInd, "++)")
    needBracket=TRUE
  }
  res=list(loopDef=loopDef,loopCode=loopCode,endCode=endCode,needBracket=needBracket)
  return(res)
}

detectBracketRequirement<-function(loopCode0,loopCode1,loopCode2){
  brackNeeded=!vapply(list(loopCode0,loopCode1,loopCode2),isEmptyCode,logical(1))
  
}
#Check if the code is empty(Comments are considered as empty)
isEmptyCode<-function(code){
  if(is.null(code)) return(TRUE)
  code1=sapply(code,function(x)gsub("//.*\n","",x))
  code1=sapply(code1,function(x)gsub("//.*","",x))
  code1=paste0(code1,collapse = "")
  code1=gsub(" ", "", code1, fixed = TRUE)
  if(code1=="")
    return(TRUE)
  else{
    return(FALSE)
  }
  
}

matrix_assignment_func_doNothing <- function(value_left, value_right) {
    paste0(value_left, "=", value_right)
}




