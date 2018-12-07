#Dispatch the assignment expression to the destinated function
#If the function is called in the left expression, func<- will be called
#If the function is called in the right expression, <-func will be called
#otherwise, C_assignment_symbols will be called
C_assignment_dispatch<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  
  #If the left expression is a function call
  #The right expression must be a symbol
  if(is.call(leftExp)){
    funcName=deparse(leftExp[[1]])
    funcName=paste0(funcName,"<-")
    func=.cFuncs[[funcName]]
    if(!is.null(func)){
      code=func(varInfo,Exp)
      return(code)
    }else{
      stop("Unsupported function: ",deparse(Exp))
    }
  }
  #If the right expression is a function call
  #The left expression must be a symbol
  if(is.call(rightExp)){
    funcName=deparse(rightExp[[1]])
    funcName=paste0("<-",funcName)
    func=.cFuncs[[funcName]]
    if(!is.null(func)){
      code=func(varInfo,Exp)
      return(code)
    }else{
      stop("Unsupported function: ",deparse(Exp))
    }
  }
  #If the left expression is not a symbol, throw an error
  if(!is.symbol(leftExp)){
    stop("Unsupported function: ",deparse(Exp))
  }
  #call the assignment function
  #Both side should be either a symbol or a number
  code=C_assignment_symbols(varInfo,Exp)
  return(code)
}





#Both side is a symbol or a number
#Exp=parse(text="A=1")[[1]]
C_assignment_symbols<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  if(isNumeric(leftExp)){
    stop("The left expression cannot be a constant: ",deparse(Exp))
  }
  leftDataType=getVarProperty(varInfo,leftExp,"dataType")
  if(leftDataType==T_scale){
    code_left=R_expression_sub(varInfo,leftExp,1)
    code_right=R_expression_sub(varInfo,rightExp,1)
    code=c(code_left$extCode,
           code_right$extCode,
           paste0(code_left$value,"=",code_right$value,";")
          )
  }else{
    code_left=R_expression_sub(varInfo,leftExp,i="gpu_symbols_i",j="gpu_symbols_j",i_C=TRUE,j_C=TRUE,base=0)
    code_right=R_expression_sub(varInfo,rightExp,i="gpu_symbols_i",j="gpu_symbols_j",i_C=TRUE,j_C=TRUE,base=0)
    code=c(
      paste0("for(",GPUVar$default_index_type," gpu_symbols_i=0;gpu_symbols_i<",
             R_nrow(varInfo,leftExp),";gpu_symbols_i++){"),
      paste0("for(",GPUVar$default_index_type," gpu_symbols_j=0;gpu_symbols_j<",
             R_ncol(varInfo,leftExp),";gpu_symbols_j++){"),
      code_left$extCode,
      code_right$extCode,
      paste0(code_left$value,"=",code_right$value,";"),
      "}",
      "}"
    )
  }
  return(code)
}


C_arithmaticOP_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  
  rightExp=Exp[[3]]
  
  leftEle=rightExp[[2]]
  rightEle=rightExp[[3]]
  op=deparse(rightExp[[1]])
    
  leftDataType=getVarProperty(varInfo,leftVar,"dataType")
  if(leftDataType==T_scale){
    loopCode=NULL
    
    code_left=R_expression_sub(varInfo,leftVar,i=1)
    code_leftEle=R_expression_sub(varInfo,leftEle,i=1)
    code_rightEle=R_expression_sub(varInfo,rightEle,i=1)
    
    endCode=NULL
  }else{
    loopCode=c(
      paste0("for(", GPUVar$default_index_type," gpu_arithmatic_i=0;gpu_arithmatic_i<",R_nrow(varInfo,leftVar),";gpu_arithmatic_i++){"),
      paste0("for(", GPUVar$default_index_type," gpu_arithmatic_j=0;gpu_arithmatic_j<",R_ncol(varInfo,leftVar),";gpu_arithmatic_j++){")
    )
    
    code_left=R_expression_sub(varInfo,leftVar,i="gpu_arithmatic_i",j="gpu_arithmatic_j",i_C =TRUE,j_C=TRUE,base=0)

    code_leftEle=R_expression_sub(varInfo,leftEle,i="gpu_arithmatic_i",j="gpu_arithmatic_j",i_C =TRUE,j_C=TRUE,base=0)
    code_rightEle=R_expression_sub(varInfo,rightEle,i="gpu_arithmatic_i",j="gpu_arithmatic_j",i_C =TRUE,j_C=TRUE,base=0)
   
    endCode=c(
      "}",
      "}")
  }
  
  value_left=code_left$value
  value_right=paste0(code_leftEle$value,op,code_rightEle$value)
  extCode=c(code_left$extCode,code_leftEle$extCode,code_rightEle$extCode)
  
  #If the operation is division, it need to be cast to a floating number
  if(op=="/")
    value_right=paste0("(",gpuMagic.option$getDefaultFloat(),")",value_right)
  
  code=c(loopCode,
         extCode,
         paste0(value_left,"=",value_right,";"),
         endCode)
  code
}

#Exp=parse(text="A=matrix(1,10,1)")[[1]]
C_matrix_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  args=matchFunArg(matrix,rightExp)
  rightVar=args$data
  
  code=C_general_matrix_assignment(varInfo,leftVar,rightVar,rightBound=R_length(varInfo,rightVar))
  
  return(code)
}
#Exp=parse(text="gpu_tmp_1=length(A)")[[1]]
C_length_left_right<-function(varInfo,Exp){
  length_func<-function(varInfo,Exp){
    return(R_length(varInfo,Exp[[2]]))
  }
  code=C_general_scalar_assignment(varInfo,Exp,"length",length_func)
  return(code)
}

#Exp=parse(text="A=nrow(A)")[[1]]
C_nrow_left_right<-function(varInfo,Exp){
  nrow_func<-function(varInfo,Exp){
    return(R_nrow(varInfo,Exp[[2]]))
  }
  code=C_general_scalar_assignment(varInfo,Exp,"nrow",nrow_func)
  return(code)
}
#Exp=parse(text="A=ncol(A)")[[1]]
C_ncol_left_right<-function(varInfo,Exp){
  ncol_func<-function(varInfo,Exp){
    return(R_ncol(varInfo,Exp[[2]]))
  }
  code=C_general_scalar_assignment(varInfo,Exp,"ncol",ncol_func)
  return(code)
}


#assigning the matrix subset to a variable
###############I NEED A DIMENSION CHECK####################
#tmp1=subRef(A,1,)
#Exp=parse(text="A=B[tmp1,]")[[1]]
C_subset_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  code=C_general_matrix_assignment(varInfo,leftVar,rightExp)
  code
}



#Exp=parse(text="A=floor(B)")[[1]]
C_floor_right<-function(varInfo,Exp){
  floorFunc<-function(left,right){
    paste0(left,"=floor(",right,")")
  }
  leftVar=Exp[[2]]
  rightVar=Exp[[3]][[2]]
  rightSize=R_length(varInfo,rightVar)
  code=C_general_matrix_assignment(varInfo,leftVar,rightVar,rightBound = rightSize)
  code
}
C_ceil_right<-function(varInfo,Exp){
  floorFunc<-function(left,right){
    paste0(left,"=ceil(",right,")")
  }
  leftVar=Exp[[2]]
  rightVar=Exp[[3]][[2]]
  rightSize=R_length(varInfo,rightVar)
  code=C_general_matrix_assignment(varInfo,leftVar,rightVar,rightBound = rightSize)
  code
}
#Exp=parse(text="return(tmp1)")[[1]]
C_return<-function(varInfo,Exp){
  returnVar=Exp[[2]]
  
  code_right=R_expression_sub(varInfo,returnVar,"gpu_return_i","gpu_return_j",i_C =T,j_C=T,base=0)
  curCode=c(
          "{",
          paste0(GPUVar$default_index_type," gpu_return_k=0;"),
          paste0("for(",GPUVar$default_index_type," gpu_return_i=0;gpu_return_i<",R_nrow(varInfo,returnVar),";gpu_return_i++){\n"),
          paste0("for(",GPUVar$default_index_type," gpu_return_j=0;gpu_return_j<",R_ncol(varInfo,returnVar),";gpu_return_j++){\n"),
          code_right$extCode,
          paste0(GPUVar$return_variable,"[gpu_return_k+",GPUVar$gpu_global_id,"*",GPUVar$return_size
                 ,"]=",
                 code_right$value,";"),
          "gpu_return_k=gpu_return_k+1;",
          paste0("if(gpu_return_k==",GPUVar$return_size,"){"),
          "break;",
          "}",
          "}",
          "}",
          "}"
          )
  curCode
}
C_break<-function(varInfo,Exp){
  "break;"
}
C_next<-function(varInfo,Exp){
  "continue;"
}

C_NULL<-function(varInfo,Exp){
  return("")
}

C_message<-function(varInfo,Exp){
  varName=Exp[[2]]
  curInfo=getVarInfo(varInfo,varName)
  if(curInfo$precisionType%in% c("double","float","half")){
    printType="%f"
  }else{
    printType="%d"
    }
  
    size1=R_nrow(varInfo,varName)
    size2=R_ncol(varInfo,varName)
    subsetCode=R_expression_sub(varInfo,returnVar,"gpu_msg_i","gpu_msg_j",i_C =T,j_C=T,base=0)

    code=c(paste0("for(uint gpu_msg_i=0;gpu_msg_i<",size1,";gpu_msg_i++){"),
           paste0("for(uint gpu_msg_j=0;gpu_msg_j<",size2,";gpu_msg_j++){"),
           subsetCode$extCode,
           paste0("printf(\"",printType,"  \",",subsetCode$value,");"),
           "}",
           "printf(\"\\n\");",
           "}"
    )
    
}

C_setVersion<-function(varInfo,Exp){
  varName=deparse(Exp[[2]])
  version=as.numeric(Exp[[3]])
  varInfo$curVarVersion[[varName]]=version
  return("")
}

C_matMul1<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]

  code=c(
    "{",
    paste0(gpuMagic.option$getDefaultFloat()," gpu_matMul_tmp;"),
    paste0("for(",GPUVar$default_index_type,
           " gpu_matMul_i=1;gpu_matMul_i<=",R_nrow(varInfo,rightVar1),
           ";gpu_matMul_i++){"),
    paste0("for(",GPUVar$default_index_type,
           " gpu_matMul_j=1;gpu_matMul_j<=",R_ncol(varInfo,rightVar2),
           ";gpu_matMul_j++){"),
    paste0("gpu_matMul_tmp=0;"),
    paste0("for(",GPUVar$default_index_type,
           " gpu_matMul_k=1;gpu_matMul_k<=",R_ncol(varInfo,rightVar1),
           ";gpu_matMul_k++){"),
    paste0("gpu_matMul_tmp=gpu_matMul_tmp+",
           R_expression_sub(varInfo,rightVar1,"gpu_matMul_i","gpu_matMul_k",i_C=TRUE,j_C=TRUE),
           "*",
           R_expression_sub(varInfo,rightVar2,"gpu_matMul_k","gpu_matMul_j",i_C=TRUE,j_C=TRUE),
           ";"),
    "}",
    paste0(R_expression_sub(varInfo,leftVar,"gpu_matMul_i","gpu_matMul_j",i_C=TRUE,j_C=TRUE),
           "=",
           "gpu_matMul_tmp;"),
    "}",
    "}",
    "}"
  )
  
  
  
  code
}


C_matMul_right<-function(varInfo,Exp){
    leftVar=Exp[[2]]
    rightExp=Exp[[3]]
    rightVar1=rightExp[[2]]
    rightVar2=rightExp[[3]]
    defaultFloat=gpuMagic.option$getDefaultFloat()
    defaultIndex=GPUVar$default_index_type
    privateSize=GPUVar$private_size
    
    leftSub=R_expression_sub(varInfo,leftVar,"gpu_i","gpu_j",i_C=T,j_C=T,base=0)
    code_right1_private=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_k+gpu_t*",privateSize),i_C=T,j_C=T,base=0)
    code_right2_multi=R_expression_sub(varInfo,rightVar2,paste0("gpu_k+gpu_t*",privateSize),"gpu_j",i_C=T,j_C=T,base=0)
    code=c(
      "{",
      paste0(defaultIndex," A_row=",R_nrow(varInfo,rightVar1),";"),
      paste0(defaultIndex," A_col=",R_ncol(varInfo,rightVar1),";"),
      paste0(defaultIndex," B_col=",R_ncol(varInfo,rightVar2),";"),
      paste0(defaultFloat," gpu_private_spcae","[",privateSize,"];"),
      paste0(defaultIndex," gpu_loopNum=ceil((",defaultFloat,")A_col/",privateSize,");"),
      paste0(defaultIndex," gpu_start=0;"),
      paste0(defaultIndex," gpu_end=0;"),
      paste0(defaultIndex, " gpu_length=0;"),
      paste0("for(",defaultIndex," gpu_t=0;gpu_t<gpu_loopNum;gpu_t++){"),
      "gpu_start=gpu_end;",
      paste0("gpu_end=gpu_end+",privateSize,";"),
      paste0("if(gpu_end>A_col) gpu_end=A_col;"),
      "gpu_length=gpu_end-gpu_start;",
      paste0("for(",defaultIndex," gpu_i=0;gpu_i<A_row;gpu_i++){"),
      paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
      code_right1_private$extCode,
      paste0("gpu_private_spcae[gpu_k]=",code_right1_private$value,";"),
      "}",
      paste0("for(",defaultIndex," gpu_j=0;gpu_j<B_col;gpu_j++){"),
      paste0(defaultFloat," gpu_tmp=0;"),
      paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
      code_right2_multi$extCode,
      paste0("gpu_tmp=gpu_tmp+gpu_private_spcae[gpu_k]*",
             code_right2_multi$value,";"),
      "}",
      "if(gpu_t==0){",
      leftSub$extCode,
      paste0(leftSub$value,"=gpu_tmp;"),
      "}else{",
      leftSub$extCode,
      paste0(leftSub$value,"=",leftSub$value,"+gpu_tmp;"),
      "}",
      "}",
      "}",
      "}",
      "}"
    )
}



