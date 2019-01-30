#Dispatch the assignment expression to the destinated function
#If the function is called in the left expression, func<- will be called
#If the function is called in the right expression, <-func will be called
#otherwise, C_assignment_symbols will be called
C_assignment_dispatch<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  
  #If the left expression is a function call
  #The right expression must be a symbol
  if(is.call(leftExp)&&!isNumeric(leftExp)){
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
  if(is.call(rightExp)&&!isNumeric(rightExp)){
    funcName=deparse(rightExp[[1]])
    #check if it is element operation
    if(funcName%in% .elementOp){
      code=C_element_OP(varInfo,Exp)
      return(code)
    }
    
    funcName=paste0("<-",funcName)
    func=.cFuncs[[funcName]]
    if(!is.null(func)){
      code=func(varInfo,Exp)
      return(code)
    }else{
      stop("Unsupported function: ",deparse(Exp))
    }
  }
  
  #call the assignment function
  #Both side should be either a symbol or a number
  code=C_assignment_symbols(varInfo,Exp)
  return(code)
}

#==================element functions===========================


C_element_OP<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  
  leftInfo=getVarInfo(varInfo,leftExp)
  if(leftInfo$dataType==T_scale){
    leftElement=C_element_getCExp(varInfo,leftExp,"1","1")
    rightElement=C_element_getCExp(varInfo,rightExp,"1","1",extCode=leftElement$extCode)
  }else{
    leftElement=C_element_getCExp(varInfo,leftExp,"gpu_element_i","gpu_element_j")
    rightElement=C_element_getCExp(varInfo,rightExp,"gpu_element_i","gpu_element_j",extCode=leftElement$extCode)
  }
  
  optCode=finalizeExtCode(rightElement$extCode)
  
  assignmentCode=paste0(leftElement$value,"=",rightElement$value,";")
  if(leftInfo$dataType==T_scale){
    if(is.null(optCode))
      code=assignmentCode
    else
      code=c("{",
             optCode,
             assignmentCode,
             "}"
             )
  }else{
    code=C_matrix_assignment(assignmentCode,
                             loopInd1 = "gpu_element_j",loopEnd1 = R_ncol(varInfo,leftExp),
                             loopInd2 = "gpu_element_i",loopEnd2 = R_nrow(varInfo,leftExp),
                             loopCode1 = optCode)
  }
  return(code)
}
#Return:
#list:value,extCode
C_element_getCExp<-function(varInfo,Exp,i,j,extCode=NULL){
  if(isNumeric(Exp)){
    res=list(value=toCharacter(Exp),extCode=extCode)
    return(res)
  }
  
  if(is.symbol(Exp)||Exp[[1]]=="["){
    res=R_expression_sub(varInfo,Exp,i,j,i_C=TRUE,j_C=TRUE,base=0,opt=TRUE)
    if(!is.null(res$colOffset)){
      if(res$colOffset!=j){
        var_res=getVarFromExtCode(extCode,GPUVar$default_index_type,res$colOffset)
        extCode=var_res$extCode
        res=R_expression_sub(varInfo,Exp,i,j,i_C=TRUE,j_C=TRUE,base=0,optCode = list(colVar=var_res$var))
      }
    }
    res$extCode=extCode
    return(res)
  }
  
  func=paste0("<-",deparse(Exp[[1]]))
  C_func=.cFuncs[[func]]
  if(!is.null(C_func)){
    res=C_func(varInfo,Exp,i,j,extCode)
    return(res)
  }
  stop("Unsupported function: ",deparse(Exp))
  
}


C_element_arithmatic<-function(varInfo,Exp,i,j,extCode){
  op=deparse(Exp[[1]])
  leftEle=Exp[[2]]
  rightEle=Exp[[3]]
  left_res=C_element_getCExp(varInfo,leftEle,i,j,extCode=extCode)
  right_res=C_element_getCExp(varInfo,rightEle,i,j,extCode=left_res$extCode)
  
  extCode=right_res$extCode
  value=paste0(left_res$value,op,right_res$value)
  if(op=="/")
    value=paste0("(",GPUVar$default_float,")",value)
  res=list(value=value,extCode=extCode)
  return(res)
}

C_element_floor<-function(varInfo,Exp,i,j,extCode){
  element=Exp[[2]]
  res=C_element_getCExp(varInfo,element,i,j,extCode=extCode)
  res$value=paste0("floor(",res$value,")")
  return(res)
}
C_element_ceil<-function(varInfo,Exp,i,j,extCode){
  element=Exp[[2]]
  res=C_element_getCExp(varInfo,element,i,j,extCode=extCode)
  res$value=paste0("ceil(",res$value,")")
  return(res)
}



#================Regular functions==================


#Both side is a symbol or a number
#Exp=parse(text="A=1")[[1]]
C_assignment_symbols<-function(varInfo,Exp){
  code=C_element_OP(varInfo,Exp)
  return(code)
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
#Exp=parse(text="tmp=A[ind,]")[[1]]
C_subset_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  code=C_general_matrix_assignment(varInfo,leftVar,rightExp)
  code
}

#varInfo=GPUExp2$varInfo
#Exp=parse(text="return(C)")[[1]]
C_return<-function(varInfo,Exp){
  if(length(Exp)==1) return("return;")
  returnVar=Exp[[2]]
  
  
  code_right=C_element_getCExp(varInfo,returnVar,"gpu_return_i","gpu_return_j")
  
  
  loopBody=paste0(GPUVar$return_variable,"[gpu_return_k]=",
                  code_right$value,";")
  extCode=finalizeExtCode(code_right$extCode)
  endCode=c("gpu_return_k=gpu_return_k+1;",
            paste0("if(gpu_return_k==",GPUVar$return_size,"){"),
            "break;",
            "}")
  code=c(
    "{",
    paste0(GPUVar$default_index_type," gpu_return_k=0;"),
    C_matrix_assignment(loopBody,
                        loopInd1 = "gpu_return_j",loopEnd1 = R_ncol(varInfo,returnVar),
                        loopInd2 = "gpu_return_i",loopEnd2 = R_nrow(varInfo,returnVar),
                        loopCode1 = extCode,endCode=endCode
    ),
    "}")
  
  code
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
    subsetCode=R_expression_sub(varInfo,varName,"gpu_msg_i","gpu_msg_j",i_C =TRUE,j_C=TRUE,base=0)

    
    code=c(paste0("for(uint gpu_msg_i=0;gpu_msg_i<",size1,";gpu_msg_i++){"),
           paste0("for(uint gpu_msg_j=0;gpu_msg_j<",size2,";gpu_msg_j++){"),
           subsetCode$extCode,
           paste0('printf("',printType,'  ",',subsetCode$value,');'),
           "}",
           'printf("\\n");',
           "}"
    )
    
}

C_setVersion<-function(varInfo,Exp){
  varName=deparse(Exp[[2]])
  version=as.numeric(Exp[[3]])
  varInfo$curVarVersion[[varName]]=version
  return("")
}


C_seq_right<-function(varInfo,Exp){
  seq<-function(from,to,by=1){}
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  
  leftInfo=getVarInfo(varInfo,leftVar)
  args=matchFunArg(seq,rightExp)
  
  from=args$from
  to=args$to
  by=args$by
  from_C=R_oneIndex_exp_sub(varInfo,from,k=1,k_C=TRUE)
  to_C=R_oneIndex_exp_sub(varInfo,from,k=1,k_C=TRUE)
  by_C=R_oneIndex_exp_sub(varInfo,by,k=1,k_C=TRUE)
  
  extCode=c(from_C$extCode,to_C$extCode,by_C$extCode)
  
  ad=leftInfo$address
  #assign a sequence to a sequence variable
  if(leftInfo$isSeq){
    code=c(
      extCode,
      paste0(ad,"[0]=",from_C$value,";"),
      paste0(ad,"[1]=",to_C$value,";"),
      paste0(ad,"[2]=",by_C$value,";")
    )
    return(code)
  }else{
    #assign a sequence to a regular variable
    
    size1=R_nrow(varInfo,leftVar)
    subsetCode=R_oneIndex_exp_sub(varInfo,leftVar,k="gpu_seq_i",k_C =TRUE,base=0)
    
    rightExtCode=c(from_C$extCode,by_C$extCode)
    
    
    rightValue=Simplify(paste0(from_C$value,"+","gpu_seq_i*",by_C$value))
    
    code=c(
      paste0("for(",GPUVar$default_index_type,
             " gpu_seq_i=0;gpu_seq_i<",size1,
             ";gpu_seq_i++){"),
      subsetCode$extCode,
      rightExtCode,
      paste0(subsetCode$value,"=",rightValue,";"),
      "}"
    )
    return(code)
  }
}
C_oneStepSeq_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  from=rightExp[[2]]
  to=rightExp[[3]]
  code=parse(text=paste0(deparse(leftVar),"=seq(",deparse(from),",",deparse(to),")"))[[1]]
  C_seq_right(varInfo,code)
  }



#Exp=quote({C=tmp%*%B})[[2]]
C_matMul_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  
  privateSize=GPUVar$private_size
  vector_size=8
  vector_length=privateSize/vector_size
  defaultFloat=GPUVar$default_float
  defaultFloatV=paste0(GPUVar$default_float,vector_size)
  defaultIndex=GPUVar$default_index_type
  
  #define macro for the matrix dimension
  dimMacroDef=c(
    paste0("gpu_A_row ",R_nrow(varInfo,rightVar1)),
    paste0("gpu_A_col ",R_ncol(varInfo,rightVar1)),
    paste0("gpu_B_row ",R_nrow(varInfo,rightVar2)),
    paste0("gpu_B_col ",R_ncol(varInfo,rightVar2)),
    paste0("gpu_private_size ",privateSize),
    paste0("gpu_vector_size ",vector_size),
    paste0("gpu_vector_len ",vector_length)
  )
  dimMacroDef=paste0("#define ",dimMacroDef)
  
  dimMacroUndef=c("gpu_A_row",
                  "gpu_A_col",
                  "gpu_B_row",
                  "gpu_B_col",
                  "gpu_private_size",
                  "gpu_vector_size",
                  "gpu_vector_len"
  )
  dimMacroUndef=paste0("#undef ",dimMacroUndef)
  
  #support variable definition
  supportVarDef=c(
    paste0(defaultFloat," gpu_private_spcae[gpu_private_size];"),
    paste0(defaultFloatV,"* gpu_private_spcae_vector=gpu_private_spcae;"),
    paste0(defaultIndex," gpu_loopNum=ceil((",defaultFloat,")gpu_A_col/gpu_private_size);"),
    paste0(defaultIndex," gpu_start=0;"),
    paste0(defaultIndex," gpu_end=0;"),
    paste0(defaultIndex, " gpu_length=0;")
  )
  #C=A%*%B
  #optimize the left matrix A
  A_opt_code=C_matMul_right_A(varInfo,Exp)
  #optimize the right matrix B
  B_opt_code=C_matMul_right_B(varInfo,Exp)
  code=c(
    "{",
    dimMacroDef,
    supportVarDef,
    paste0("if(",R_nrow(varInfo,rightVar1),">",R_ncol(varInfo,rightVar2),"){"),
    B_opt_code,
    "}else{",
    A_opt_code,
    "}",
    dimMacroUndef,
    "}"
  )
  code
}

#Store the row of A
C_matMul_right_A<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  privateSize=GPUVar$private_size
  vector_size=8
  vector_length=privateSize/vector_size
  defaultFloat=GPUVar$default_float
  defaultFloatV=paste0(GPUVar$default_float,vector_size)
  defaultIndex=GPUVar$default_index_type
  
  #private assignment
  A_sub_private_tmp=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_start+gpu_k"),i_C=TRUE,j_C=TRUE,base=0,opt = TRUE)
  A_row_var=GPUVar$getTmpVar()
  A_row_code=paste(defaultIndex," ",A_row_var,"=",A_sub_private_tmp$rowOffset,";")
  A_sub_private=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_start+gpu_k"),i_C=TRUE,j_C=TRUE,
                                 base=0,optCode = list(rowVar=A_row_var,colVar=A_sub_private_tmp$colOffset))
  private_assign=c(
    "//Read a piece of row of A into the private memory",
    A_row_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
    A_sub_private$extCode,
    paste0("gpu_private_spcae[gpu_k]=",A_sub_private$value,";"),
    "}"
  )
  
  #matrix multiplication in scalar format
  B_multi_sub_tmp=R_expression_sub(varInfo,rightVar2,"gpu_k+gpu_start","gpu_j",
                                   i_C=TRUE,j_C=TRUE,base=0,opt = TRUE)
  B_col_var=GPUVar$getTmpVar()
  B_col_code=paste(defaultIndex," ",B_col_var,"=",B_multi_sub_tmp$colOffset,";")
  B_multi_sub=R_expression_sub(varInfo,rightVar2,"gpu_k+gpu_start","gpu_j",
                               i_C=TRUE,j_C=TRUE,base=0,
                               optCode = list(rowVar=B_multi_sub_tmp$rowOffset,colVar=B_col_var))
  
  matrixMultiScalar=c(
    "//Element operation to compute the matrix multiplication",
    paste0(defaultFloat," gpu_tmp=0;"),
    B_col_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
    B_multi_sub$extCode,
    paste0("gpu_tmp=gpu_tmp+gpu_private_spcae[gpu_k]*",
           B_multi_sub$value,";"),
    "}"
  )
  
  #result writing back in scalar format
  #result assignment
  res_leftSub=R_expression_sub(varInfo,leftVar,"gpu_i","gpu_j",i_C=TRUE,j_C=TRUE,base=0)
  
  writeBackResScalar=c(
    "//Write the result back to the matrix",
    "if(gpu_t==0){",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=gpu_tmp;"),
    "}else{",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",res_leftSub$value,"+gpu_tmp;"),
    "}"
  )
  
  #B_vector
  B_code_tmp=R_expression_sub(varInfo,rightVar2,paste0("gpu_k*gpu_vector_size+gpu_start"),"gpu_j",
                              i_C=TRUE,j_C=TRUE,base=0,opt=TRUE)
  B_col_var=GPUVar$getTmpVar()
  B_col_code=paste(defaultIndex," ",B_col_var,"=",B_code_tmp$colOffset,";")
  
  B_vec=c()
  B_ext=c()
  for(i in seq_len(vector_size)){
    B_code_tmp=R_expression_sub(varInfo,rightVar2,paste0("gpu_k*gpu_vector_size+gpu_start+",i-1),"gpu_j",
                                i_C=TRUE,j_C=TRUE,base=0,opt=TRUE)
    B_code=R_expression_sub(varInfo,rightVar2,paste0("gpu_k*gpu_vector_size+gpu_start+",i-1),"gpu_j",
                            i_C=TRUE,j_C=TRUE,base=0,
                            optCode=list(rowVar=B_code_tmp$rowOffset,colVar=B_col_var))
    B_vec=c(B_vec,B_code$value)
    B_ext=c(B_ext,B_code$extCode)
  }
  B_vector_code=paste0(defaultFloatV," gpu_B_vector=(",defaultFloatV,")(",
                       paste0(B_vec,collapse = ","),");")
  
  
  #matrix multiplication in vector format
  matrixMultiVector=c(
    paste0(defaultFloatV," gpu_tmp=0;"),
    B_col_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_vector_len;gpu_k++){"),
    B_ext,
    B_vector_code,
    paste0("gpu_tmp=gpu_tmp+gpu_private_spcae_vector[gpu_k]*gpu_B_vector;"),
    "}"
  )
  
  
  
  #temp vector Sum
  temp_vector_sum=paste0(paste0("gpu_tmp.s",0:(vector_size-1)),collapse = "+")
  
  #result writing back in vector format
  writeBackResVector=c(
    "//Write the result back to the matrix",
    "if(gpu_t==0){",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",temp_vector_sum,";"),
    "}else{",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",res_leftSub$value,"+",temp_vector_sum,";"),
    "}"
  )
  
  
  code_right2_multi=R_expression_sub(varInfo,rightVar2,paste0("gpu_k+gpu_t*",privateSize),"gpu_j",i_C=TRUE,j_C=TRUE,base=0)
  code=c(
    paste0("for(",defaultIndex," gpu_t=0;gpu_t<gpu_loopNum;gpu_t++){"),
    "gpu_start=gpu_end;",
    paste0("gpu_end=gpu_end+gpu_private_size;"),
    paste0("if(gpu_end>gpu_A_col) gpu_end=gpu_A_col;"),
    "gpu_length=gpu_end-gpu_start;",
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    private_assign,
    paste0("if(gpu_length!=gpu_private_size){"),
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    matrixMultiScalar,
    writeBackResScalar,
    "}",
    "}else{",
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    matrixMultiVector,
    writeBackResVector,
    "}",
    "}",
    "}",
    "}"
  )
  code
}


#Store the column of B
C_matMul_right_B<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  privateSize=GPUVar$private_size
  vector_size=8
  vector_length=privateSize/vector_size
  defaultFloat=GPUVar$default_float
  defaultFloatV=paste0(GPUVar$default_float,vector_size)
  defaultIndex=GPUVar$default_index_type
  
  #private assignment
  B_sub_private_tmp=R_expression_sub(varInfo,rightVar2,paste0("gpu_start+gpu_k"),"gpu_j",i_C=TRUE,j_C=TRUE,base=0,opt = TRUE)
  B_col_var=GPUVar$getTmpVar()
  B_col_code=paste(defaultIndex," ",B_col_var,"=",B_sub_private_tmp$colOffset,";")
  B_sub_private=R_expression_sub(varInfo,rightVar2,paste0("gpu_start+gpu_k"),"gpu_j",i_C=TRUE,j_C=TRUE,
                                 base=0,optCode = list(rowVar=B_sub_private_tmp$rowOffset,colVar=B_col_var))
  private_assign=c(
    "//Read a piece of column of B into the private memory",
    B_col_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
    B_sub_private$extCode,
    paste0("gpu_private_spcae[gpu_k]=",B_sub_private$value,";"),
    "}"
  )
  
  #matrix multiplication in scalar format
  A_multi_sub_tmp=R_expression_sub(varInfo,rightVar1,"gpu_i","gpu_k+gpu_start",
                                   i_C=TRUE,j_C=TRUE,base=0,opt = TRUE)
  A_row_var=GPUVar$getTmpVar()
  A_row_code=paste(defaultIndex," ",A_row_var,"=",A_multi_sub_tmp$rowOffset,";")
  A_multi_sub=R_expression_sub(varInfo,rightVar1,"gpu_i","gpu_k+gpu_start",
                               i_C=TRUE,j_C=TRUE,base=0,
                               optCode = list(rowVar=A_row_var,colVar=A_multi_sub_tmp$colOffset))
  
  matrixMultiScalar=c(
    "//Element operation to compute the matrix multiplication",
    paste0(defaultFloat," gpu_tmp=0;"),
    A_row_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_length;gpu_k++){"),
    A_multi_sub$extCode,
    paste0("gpu_tmp=gpu_tmp+gpu_private_spcae[gpu_k]*",
           A_multi_sub$value,";"),
    "}"
  )
  
  #result writing back in scalar format
  #result assignment
  res_leftSub=R_expression_sub(varInfo,leftVar,"gpu_i","gpu_j",i_C=TRUE,j_C=TRUE,base=0)
  
  writeBackResScalar=c(
    "//Write the result back to the matrix",
    "if(gpu_t==0){",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=gpu_tmp;"),
    "}else{",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",res_leftSub$value,"+gpu_tmp;"),
    "}"
  )
  
  #A_vector
  A_code_tmp=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_k*gpu_vector_size+gpu_start"),
                              i_C=TRUE,j_C=TRUE,base=0,opt=TRUE)
  A_row_var=GPUVar$getTmpVar()
  A_row_code=paste(defaultIndex," ",A_row_var,"=",A_code_tmp$rowOffset,";")
  
  A_vec=c()
  A_ext=c()
  for(j in seq_len(vector_size)){
    A_code_tmp=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_k*gpu_vector_size+gpu_start+",j-1),
                                i_C=TRUE,j_C=TRUE,base=0,opt=TRUE)
    A_code=R_expression_sub(varInfo,rightVar1,"gpu_i",paste0("gpu_k*gpu_vector_size+gpu_start+",j-1),
                            i_C=TRUE,j_C=TRUE,base=0,
                            optCode=list(rowVar=A_row_var,colVar=A_code_tmp$colOffset))
    A_vec=c(A_vec,A_code$value)
    A_ext=c(A_ext,A_code$extCode)
  }
  A_vector_code=paste0(defaultFloatV," gpu_A_vector=(",defaultFloatV,")(",
                       paste0(A_vec,collapse = ","),");")
  
  
  #matrix multiplication in vector format
  matrixMultiVector=c(
    paste0(defaultFloatV," gpu_tmp=0;"),
    A_row_code,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_vector_len;gpu_k++){"),
    A_ext,
    A_vector_code,
    paste0("gpu_tmp=gpu_tmp+gpu_private_spcae_vector[gpu_k]*gpu_A_vector;"),
    "}"
  )
  
  
  
  #temp vector Sum
  temp_vector_sum=paste0(paste0("gpu_tmp.s",0:(vector_size-1)),collapse = "+")
  
  #result writing back in vector format
  writeBackResVector=c(
    "//Write the result back to the matrix",
    "if(gpu_t==0){",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",temp_vector_sum,";"),
    "}else{",
    res_leftSub$extCode,
    paste0(res_leftSub$value,"=",res_leftSub$value,"+",temp_vector_sum,";"),
    "}"
  )
  
  code=c(
    paste0("for(",defaultIndex," gpu_t=0;gpu_t<gpu_loopNum;gpu_t++){"),
    "gpu_start=gpu_end;",
    paste0("gpu_end=gpu_end+gpu_private_size;"),
    paste0("if(gpu_end>gpu_A_col) gpu_end=gpu_A_col;"),
    "gpu_length=gpu_end-gpu_start;",
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    private_assign,
    paste0("if(gpu_length!=gpu_private_size){"),
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    matrixMultiScalar,
    writeBackResScalar,
    "}",
    "}else{",
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    matrixMultiVector,
    writeBackResVector,
    "}",
    "}",
    "}",
    "}"
  )
  code
}




