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
  
  extCode=c(extCode,code_left$extCode)
  value_left=code_left$value
  
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
  
  extCode=c(extCode,code_right$extCode)
  value_right=code_right$value
  
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
    
    code_left=C_element_getCExp(varInfo,leftVar,"0","0")
    code_right=C_element_getCExp(varInfo,rightVar,"0","0",extCode=code_left$extCode)
    extCode=c(code_left$extCode,code_right$extCode)
    code=paste0(extCode,
      func(code_left$value,code_right$value),";")
    
  }else{
    code_right=R_oneIndex_exp_sub(varInfo,rightVar,k="gpu_general_index",k_C=TRUE,base=0)
    #dispatch accoding to if the right matrix has boundary
    #if the right matrix is a number, boundary will be ignored
    if(is.null(rightBound)||
       length(grep("gpu_general_index",code_right$value,fixed = TRUE))==0){
      
      i="gpu_general_index_i"
      j="gpu_general_index_j"
      code_left=C_element_getCExp(varInfo,leftVar,i,j)
      code_right=C_element_getCExp(varInfo,rightVar,i,j,extCode=code_left$extCode)
      bodyCode=paste0(func(code_left$value,code_right$value),";")
        
      code=C_matrix_assignment(bodyCode,
                               loopInd1 =j,loopEnd1 =R_ncol(varInfo,leftVar),
                               loopInd2=i,loopEnd2=R_nrow(varInfo,leftVar),
                               loopCode1 = code_right$extCode)
      
    }else{
      code_left=R_oneIndex_exp_sub(varInfo,leftVar,k=="gpu_general_index_i",k_C=TRUE,base=0)
      code_right=R_oneIndex_exp_sub(varInfo,rightVar,k="gpu_general_index_k",k_C=TRUE,base=0)
      
      bodyCode=paste0(func(code_left$value,code_right$value),";")
      extCode=c(code_left$extCode,code_right$extCode)
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
                            loopCode1=extCode,endCode = endCode),
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
#endCode
#}
#}
C_matrix_assignment<-function(bodyCode,
                              loopInd1,loopStart1="0",loopEnd1,
                              loopInd2=NULL,loopStart2="0",loopEnd2=NULL,
                              loopCode1=NULL,loopCode2=NULL,endCode=NULL){
  code=
  code=c(
    paste0("for(",GPUVar$default_index_type," ",loopInd1,"=",loopStart1,
           ";",loopInd1,"<",loopEnd1,";",loopInd1,"++){"),
    loopCode1
  )
  if(!is.null(loopEnd2)){
    code=c(code,
           paste0("for(",GPUVar$default_index_type," ",loopInd2,"=",loopStart2,
                  ";",loopInd2,"<",loopEnd2,";",loopInd2,"++){"),
           loopCode2)
    endCode=c(endCode,"}")
  }
  code=c(code,
         bodyCode,
         endCode,
         "}"
  )
  code
}

matrix_assignment_func_doNothing<-function(value_left,value_right){
  paste0(value_left,"=",value_right)
}
