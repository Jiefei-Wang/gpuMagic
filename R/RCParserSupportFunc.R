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
#' @family internal cast functions
#' @export
gpu_cast_float<-function(x){
  as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @family internal cast functions
#' @export
gpu_cast_double<-function(x){
  as.double(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @family internal cast functions
#' @export
gpu_cast_uint<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @family internal cast functions
#' @export
gpu_cast_int<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @family internal cast functions
#' @export
gpu_cast_long<-function(x){
  trunc(x)
}
#' Internal usage only, the package export this function only for the other package to access.
#' 
#' @param x Internal usage only
#' @family internal cast functions
#' @export
gpu_cast_ulong<-function(x){
  trunc(x)
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
      code_left=func[[which(funcName==deparse(leftExp[[1]]))]](varInfo,leftExp)
    }else{
      stop("Unexpected function:", deparse(Exp))
    }
  }else{
    code_left=R_expression_sub(varInfo,leftExp,1)
    
  }
  
  if(is.list(code_left)){
    extCode=c(extCode,code_left$extCode)
    value_left=code_left$value
  }else{
    value_left=code_left
  }
  
  
  #if the right expression is the length function
  if(is.call(rightExp)){
    if(deparse(rightExp[[1]])%in%funcName){
      code_right=func[[which(funcName==deparse(rightExp[[1]]))]](varInfo,rightExp)
    }else{
      stop("Unexpected function:", deparse(Exp))
    }
  }else{
    code_right=R_expression_sub(varInfo,rightExp,1)
  }
  
  if(is.list(code_right)){
    extCode=c(extCode,code_right$extCode)
    value_right=code_right$value
  }else{
    value_right=code_right
  }
  
  code=paste0(value_left,"=",value_right,";")
  
  return(c(extCode,code))
}
#This function is for the general matrix assignment
#The left and right variable should be able to be directly processed by the oneIndex_sub function
#at the final stage, a func will be called with two parameters: value_left and value_right
#to do some special treatment for each element
C_general_matrix_assignment<-function(varInfo,leftVar,rightVar,func=matrix_assignment_func_doNothing,rightBound=NULL){
  leftDataType=getVarProperty(varInfo,leftVar,"dataType")
  if(leftDataType==T_scale){
    loopCode=NULL
    
    code_left=R_oneIndex_exp_sub(varInfo,leftVar,k=1)
    code_right=R_oneIndex_exp_sub(varInfo,rightVar,k=1)
    
    endCode=NULL
  }else{
    code_right=R_oneIndex_exp_sub(varInfo,rightVar,k="gpu_general_index",k_C=TRUE,base=0)
    #dispatch accoding to if the right matrix has boundary
    #if the right matrix is a number, boundary will be ignored
    if(is.null(rightBound)||
       length(grep("gpu_general_index",code_right$value,fixed = TRUE))==0){
      loopCode=paste0("for(", GPUVar$default_index_type," gpu_general_index=0;gpu_general_index<",R_length(varInfo,leftVar),";gpu_general_index++){")
      
      code_left=R_oneIndex_exp_sub(varInfo,leftVar,k="gpu_general_index",k_C=TRUE,base=0)
      #right code is defined above
      #code_right=R_oneIndex_exp_sub(varInfo,rightVar,k="gpu_general_index",k_C=TRUE,base=0)
      
      endCode="}"
    }else{
      loopCode=c(
        "{",
        paste0(GPUVar$default_index_type," gpu_general_index_k=0;"),
        paste0(GPUVar$default_index_type," gpu_right_matrix_length=",rightBound,";"),
        paste0("for(", GPUVar$default_index_type," gpu_general_index_i=0;gpu_general_index_i<",R_length(varInfo,leftVar),";gpu_general_index_i++){")
        )
      
      code_left=R_oneIndex_exp_sub(varInfo,leftVar,k="gpu_general_index_i",k_C=TRUE,base=0)
      code_right=R_oneIndex_exp_sub(varInfo,rightVar,k="gpu_general_index_k",k_C=TRUE,base=0)
      
      endCode=c(
        "gpu_general_index_k=gpu_general_index_k+1;",
        paste0("if(gpu_general_index_k==gpu_right_matrix_length){"),
        "gpu_general_index_k=0;",
        "}",
        "}",
        "}"
        )
    }
  }
  value_left=code_left$value
  value_right=code_right$value
  extCode=c(code_left$extCode,code_right$extCode)
  
  code=c(loopCode,
         extCode,
         paste0(func(value_left,value_right),";"),
         endCode)
  code
}

matrix_assignment_func_doNothing<-function(value_left,value_right){
  paste0(value_left,"=",value_right)
}
