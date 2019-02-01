#==================parser 1====================
addvariableSizeInfo<-function(sizeInfo,curVarInfo){
  curSizeinfo=data.frame(
    var=curVarInfo$var,precisionType=curVarInfo$precisionType,
    totalSize=Simplify(paste0("(",curVarInfo$totalSize,")*",getTypeSize(curVarInfo$precisionType))),
    size1=curVarInfo$size1,size2=curVarInfo$size2,
    stringsAsFactors = FALSE)
    
  
  if(curVarInfo$redirect!="NA"){
    curSizeinfo$totalSize=0
  }
  sizeInfo=rbind(sizeInfo,curSizeinfo)
  return(sizeInfo)
}
addVariableDeclaration<-function(curVarInfo,data,offset,offsetInd){
  CXXtype=curVarInfo$precisionType
  if(curVarInfo$specialType=="ref")
    return(NULL)
  if(curVarInfo$redirect=="NA"){
      curCode=paste0("global ",CXXtype,"* ",curVarInfo$var,"=",
                     "(global ",CXXtype,"*)(",
                     data
                     ,"+",
                     offset,"[",offsetInd,"]);")
  }else{
    curCode=paste0("#define ",curVarInfo$var," ",curVarInfo$redirect)
    #curCode=paste0("global ",CXXtype,"* ",curVarInfo$var,"=",curVarInfo$redirect,";")
  }
  return(curCode)
}


#==================parser 2====================
CSimplify<-function(Exp,C=TRUE){
  code=toCharacter(Exp)
  
  if(code=="") return(code)
  if(C)
    code=gsub("\\((float|double|uint|int|long|ulong)\\)","gpu_cast_\\1",code)
  code=Simplify(code)
  if(C)
    code=gsub("gpu_cast_(float|double|uint|int|long|ulong)","\\(\\1\\)",code)
  return(code)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @rdname internalFunctions
#' @return A double type data
#' @export
gpu_cast_float<-function(x){
  as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_double<-function(x){
  as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_uint<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_int<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_long<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @rdname internalFunctions
#' @export
gpu_cast_ulong<-function(x){
  trunc(x)
}
#Check if x is a valid symbol(no `` around the x)
isSymbol<-function(x){
  x=as.symbol(x)
  length(grep('`',capture.output(x)))==0
}




getSeqAddress<-function(varInfo,var){
  curInfo=getVarInfo(varInfo,var)
  ad=curInfo$address
  from=paste0(ad,"[0]")
  to=paste0(ad,"[1]")
  by=paste0(ad,"[2]")
  data.frame(from=from,to=to,by=by,stringsAsFactors = FALSE)
}




#A general function to assign a scalar to another scalar
#The funcName shoud be the only function that is allowed to call in the scalar assignment
#Exp: The assignment expression
#funcName: The name of the function that is expected in the expression
#func: The function that will be called to process the function in the expression
#Exp=parse(text="A=ncol(A)")[[1]]
#funcName="ncol"
#func=R_ncol
C_general_scalar_assignment<-function(varInfo,Exp,funcName,func){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  extCode=NULL
  if(!is.list(func)){
    func=list(func)
  }
  
  #if the left expression is the length function
  if(is.call(leftExp)){
    if(deparse(leftExp[[1]])%in%funcName){
      value_left=func[[which(funcName==deparse(leftExp[[1]]))]](varInfo,leftExp)
    }else{
      stop("Unexpected function:", deparse(Exp))
    }
  }else{
    res=R_expression_sub(varInfo,leftExp,sub=1,sub_C=TRUE,extCode=extCode)
    value_left=res$value
    extCode=res$extCode
  }
  
  
  #if the right expression is the length function
  if(is.call(rightExp)){
    if(deparse(rightExp[[1]])%in%funcName){
      code_right=func[[which(funcName==deparse(rightExp[[1]]))]](varInfo,rightExp)
    }else{
      stop("Unexpected function:", deparse(Exp))
    }
  }else{
    res=R_expression_sub(varInfo,rightExp,sub=1,sub_C=TRUE,extCode=extCode)
    value_right=res$value
    extCode=res$extCode
  }
  
  extCode=finalizeExtCode(extCode)
  code=paste0(value_left,"=",value_right,";")
  
  return(c(extCode$optCode,extCode$extraCode,code))
}
#This function is for the general matrix assignment
#The left and right variable should be able to be directly processed by the oneIndex_sub function
#at the final stage, a func will be called with two parameters: value_left and value_right
#to do some special treatment for each element
C_general_matrix_assignment<-function(varInfo,leftVar,rightVar,func=matrix_assignment_func_doNothing,rightBound=NULL){
  leftDataType=getVarProperty(varInfo,leftVar,"dataType")
  if(leftDataType==T_scale){
    sub=c(0,0)
    code_left=C_element_getCExp(varInfo,leftVar,sub=sub,opt=FALSE)
    code_right=C_element_getCExp(varInfo,rightVar,sub=sub,extCode=code_left$extCode,opt=FALSE)
    extCode=finalizeExtCode(code_right$extCode)
    code=paste0(extCode$optCode,extCode$extraCode,
      func(code_left$value,code_right$value),";")
    
  }else{
    #dispatch accoding to if the right matrix has boundary
    #if the right matrix is a number, boundary will be ignored
    if(is.null(rightBound)){
      
      i="gpu_general_index_i"
      j="gpu_general_index_j"
      sub=c(i,j)
      code_left=C_element_getCExp(varInfo,leftVar,sub=sub)
      code_right=C_element_getCExp(varInfo,rightVar,sub=sub,extCode=code_left$extCode)
      bodyCode=paste0(func(code_left$value,code_right$value),";")
      extCode=finalizeExtCode(code_right$extCode)
      
      code=C_matrix_assignment(bodyCode,
                               loopInd1 =j,loopEnd1 =R_ncol(varInfo,leftVar),
                               loopInd2=i,loopEnd2=R_nrow(varInfo,leftVar),
                               loopCode1 = extCode$optCode,loopCode2 = extCode$extraCode)
      
    }else{
      sub
      code_left=C_element_getCExp(varInfo,leftVar,sub="gpu_general_index_i",opt=FALSE)
      code_right=C_element_getCExp(varInfo,rightVar,sub="gpu_general_index_k",extCode=code_left$extCode,opt=FALSE)
      
      bodyCode=paste0(func(code_left$value,code_right$value),";")
      extCode=finalizeExtCode(code_right$extCode)
      endCode=c(
        "gpu_general_index_k=gpu_general_index_k+1;",
        paste0("if(gpu_general_index_k==gpu_right_matrix_length){"),
        "gpu_general_index_k=0;"
        )
      
      code=c(
        "{",
        paste0(GPUVar$default_index_type," gpu_general_index_k=0;"),
        paste0(GPUVar$default_index_type," gpu_right_matrix_length=",rightBound,";"),
        C_matrix_assignment(bodyCode,
                            loopInd1 ="gpu_general_index_i",loopEnd1 =R_length(varInfo,leftVar),
                            loopCode1=c(extCode$optCode,extCode$extraCode),endCode1 = endCode),
        "}"
      )
    }
  }
  
  code
}

#This function will not check the variable in the varInfo
#it just create a for loop in C code format
#for(loopStart1:loopEnd1-1){
#loopCode1
#for(loopStart2:loopEnd2-1){
#loopCode2
#bodyCode
#endCode2
#}
#endCode1
#}
C_matrix_assignment<-function(bodyCode,
                              loopInd1,loopStart1="0",loopEnd1,
                              loopInd2=NULL,loopStart2="0",loopEnd2=NULL,
                              loopCode1=NULL,loopCode2=NULL,endCode1=NULL,endCode2=NULL){
  code=
  code=c(
    paste0("for(",GPUVar$default_index_type," ",loopInd1,"=",loopStart1,
           ";",loopInd1,"<",loopEnd1,";",loopInd1,"++){"),
    loopCode1
  )
  if(!is.null(loopInd2)){
    code=c(code,
           paste0("for(",GPUVar$default_index_type," ",loopInd2,"=",loopStart2,
                  ";",loopInd2,"<",loopEnd2,";",loopInd2,"++){"),
           loopCode2)
    endCode2=c(endCode2,"}")
  }
  code=c(code,
         bodyCode,
         endCode2,
         endCode1,
         "}"
  )
  code
}

matrix_assignment_func_doNothing<-function(value_left,value_right){
  paste0(value_left,"=",value_right)
}
