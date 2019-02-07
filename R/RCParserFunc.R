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
    sub=c("1","1")
    leftElement=C_element_getCExp(varInfo,leftExp,sub=sub,opt=NULL)
    rightElement=C_element_getCExp(varInfo,rightExp,sub=sub,extCode=leftElement$extCode,opt=NULL)
  }else{
    sub=c("gpu_element_i","gpu_element_j")
    leftElement=C_element_getCExp(varInfo,leftExp,sub=sub,
                                  opt=list("gpu_element_j","gpu_element_i"))
    rightElement=C_element_getCExp(varInfo,rightExp,sub=sub,extCode=leftElement$extCode,
                                   opt=list("gpu_element_j","gpu_element_i"))
  }
  
  extCode=finalizeExtCode(rightElement$extCode)
  
  assignmentCode=paste0(leftElement$value,"=",rightElement$value,";")
  if(leftInfo$dataType==T_scale){
    extCode=unlist(extCode)
    if(is.null(extCode))
      code=assignmentCode
    else
      code=c("{",
             extCode,
             assignmentCode,
             "}"
             )
  }else{
    code=C_matrix_assignment(assignmentCode,
                             loopInd1 = "gpu_element_j",loopEnd1 = R_ncol(varInfo,leftExp),
                             loopInd2 = "gpu_element_i",loopEnd2 = R_nrow(varInfo,leftExp),
                             loopCode0=extCode$L0,loopCode1 = extCode$L1,loopCode2 = extCode$L2)
  }
  return(code)
}
#0-based index
#Return:
#list:value,extCode
C_element_getCExp<-function(varInfo,Exp,sub,extCode=NULL,opt=NULL){
  if(is.null(extCode)) extCode=createExtCode(opt)
  if(isNumeric(Exp)){
    res=list(value=toCharacter(Exp),extCode=extCode)
    return(res)
  }
  
  if(is.symbol(Exp)||Exp[[1]]=="["){
    res=R_expression_sub(varInfo,Exp,sub=sub,sub_C=TRUE,opt=opt,extCode=extCode,base=0)
    return(res)
  }
  
  func=paste0("<-",deparse(Exp[[1]]))
  C_func=.cFuncs[[func]]
  if(!is.null(C_func)){
    res=C_func(varInfo,Exp,sub,opt=opt,extCode=extCode)
    return(res)
  }
  stop("Unsupported function: ",deparse(Exp))
  
}


C_element_arithmatic<-function(varInfo,Exp,sub,opt,extCode){
  op=deparse(Exp[[1]])
  leftEle=Exp[[2]]
  rightEle=Exp[[3]]
  left_res=C_element_getCExp(varInfo,leftEle,sub,extCode=extCode,opt=opt)
  right_res=C_element_getCExp(varInfo,rightEle,sub,extCode=left_res$extCode,opt=opt)
  
  extCode=right_res$extCode
  value=paste0(left_res$value,op,right_res$value)
  if(op=="/")
    value=paste0("(",GPUVar$default_float,")",value)
  res=list(value=value,extCode=extCode)
  return(res)
}

C_element_floor<-function(varInfo,Exp,sub,opt,extCode){
  element=Exp[[2]]
  res=C_element_getCExp(varInfo,element,sub,extCode=extCode,opt=opt)
  res$value=paste0("floor(",res$value,")")
  return(res)
}
C_element_ceil<-function(varInfo,Exp,sub,opt,extCode){
  element=Exp[[2]]
  res=C_element_getCExp(varInfo,element,sub,extCode=extCode,opt=opt)
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

#Exp=parse(text="A=matrix(B,10,1)")[[1]]
C_matrix_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  args=matchFunArg(matrix,rightExp)
  rightVar=args$data
  
  code=C_general_matrix_assignment(varInfo,leftVar,rightVar,rightBound=R_length(varInfo,rightVar))
  
  return(code)
}
#Exp=parse(text="gpu_temp_var1=length(A)")[[1]]
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
#Exp=parse(text="B=A[ind,]")[[1]]
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
  
  
  code_right=C_element_getCExp(varInfo,returnVar,sub=c("gpu_return_i","gpu_return_j"),opt=list("gpu_return_j","gpu_return_i"))
  
  
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
                        loopCode0=extCode$L0,loopCode1 = extCode$L1,loopCode2=extCode$L2,endCode2=endCode
    ),
    "}",
    "return;"
    )
  
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
    subsetCode=C_element_getCExp(varInfo,varName,sub=c("gpu_msg_i","gpu_msg_j"),opt=list("gpu_msg_i","gpu_msg_j"))

    loopBody=paste0('printf("',printType,'  ",',subsetCode$value,');')
    endCode1='printf("\\n");'
    extCode=finalizeExtCode(subsetCode$extCode)
    
    code=C_matrix_assignment(loopBody,
                        loopInd1 = "gpu_msg_i",loopEnd1 = size1,
                        loopInd2 = "gpu_msg_j",loopEnd2 = size2,
                        loopCode0=extCode$L0,loopCode1 = extCode$L1,loopCode2=extCode$L2,endCode1=endCode1
    )
    code
}

C_setVersion<-function(varInfo,Exp){
  varName=deparse(Exp[[2]])
  version=as.numeric(Exp[[3]])
  varInfo$curVarVersion[[varName]]=version
  return("")
}

#Exp=parse(text="A=seq(1,10,1)")[[1]]
C_seq_right<-function(varInfo,Exp){
  seq<-function(from,to,by=1){}
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  
  leftInfo=getVarInfo(varInfo,leftVar)
  args=matchFunArg(seq,rightExp)
  
  from=args$from
  to=args$to
  by=args$by
  from_C=C_element_getCExp(varInfo,from,sub=1)
  to_C=C_element_getCExp(varInfo,to,sub=1,extCode=from_C$extCode)
  by_C=C_element_getCExp(varInfo,by,sub=1,extCode=to_C$extCode)
  
  #Manually simplify the length
  part1=CSimplify(paste0(to_C$value,"/",by_C$value))
  part2=CSimplify(paste0(from_C$value,"/",by_C$value))
  if(!xor(isNumeric(part1),isNumeric(part2))){
    seq_size=CSimplify(paste0("floor((",GPUVar$default_float,")(",part1,"-",part2,"))+1"))
  }else{
    if(isNumeric(part1)){
      seq_size=CSimplify(paste0("-floor((",GPUVar$default_float,")(",part2,"))+",part1,"+1"))
    }else{
      if(isNumeric(part2)){
        seq_size=CSimplify(paste0("floor((",GPUVar$default_float,")(",part1,"))-",part2,"+1"))
      }
    }
  }
  
  
  extCode=by_C$extCode
  
  extCode_seq=finalizeExtCode(extCode)
  #assign a sequence to a sequence variable
  if(leftInfo$isSeq){
    seqAd=getSeqAddress(varInfo,leftVar)
    
    
    code=c(
      extCode$L0,
      paste0(seqAd$from,"=",from_C$value,";"),
      paste0(seqAd$to,"=",to_C$value,";"),
      paste0(seqAd$by,"=",by_C$value,";"),
      paste0(seqAd$length,"=",seq_size,";")
    )
    
    return(code)
  }else{
    #assign a sequence to a regular variable
    size1=R_nrow(varInfo,leftVar)
    subsetCode=C_element_getCExp(varInfo,leftVar,sub=c("gpu_seq_i","gpu_seq_j"),
                                 opt=list("gpu_seq_i",c("gpu_seq_j","gpu_seq_k")))
    rightValue=CSimplify(paste0(from_C$value,"+","gpu_seq_k*",by_C$value))
    
    loopBody=paste0(subsetCode$value,"=",rightValue,";")
    endCode=c("gpu_seq_k=gpu_seq_k+1;",
              paste0("if(gpu_seq_k==",seq_size,"){"),
              "break;",
              "}")
    
    extCode=finalizeExtCode(subsetCode$extCode)
    code=c(
      "{",
      paste0(GPUVar$default_index_type," gpu_seq_k=0;"),
      C_matrix_assignment(loopBody,
                          loopInd1 = "gpu_seq_i",loopEnd1 = R_nrow(varInfo,leftVar),
                          loopInd2 = "gpu_seq_j",loopEnd2 = R_ncol(varInfo,leftVar),
                          loopCode0=extCode$L0,loopCode1 = extCode$L1,loopCode2=c(extCode$L2,extCode_seq$L0),
                          endCode2=endCode
      ),
      "}")
    return(code)
  }
}
#Exp=parse(text="A=1:10")[[1]]
C_oneStepSeq_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  from=rightExp[[2]]
  to=rightExp[[3]]
  code=parse(text=paste0(deparse(leftVar),"=seq(",deparse(from),",",deparse(to),")"))[[1]]
  C_seq_right(varInfo,code)
  }

C_transpose_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar=rightExp[[2]]
  
  leftEle=C_element_getCExp(varInfo,leftVar,sub=c("gpu_tranpose_i","gpu_tranpose_j"),opt=list("gpu_tranpose_i","gpu_tranpose_j"))
  rightEle=C_element_getCExp(varInfo,rightVar,sub=c("gpu_tranpose_j","gpu_tranpose_i"),opt=list("gpu_tranpose_i","gpu_tranpose_j"),extCode = leftEle$extCode)
  
  size1=R_nrow(varInfo,leftVar)
  size2=R_ncol(varInfo,leftVar)
  
  loopBody=paste0(leftEle$value,"=",rightEle$value,";")
  extCode=finalizeExtCode(rightEle$extCode)
  
  code=C_matrix_assignment(loopBody,
                           loopInd1 = "gpu_tranpose_i",loopEnd1 = size1,
                           loopInd2 = "gpu_tranpose_j",loopEnd2 = size2,
                           loopCode0=extCode$L0,loopCode1 = extCode$L1,loopCode2=extCode$L2
  )
  code
}




##########################################Super lengthy function##############################

#Exp=quote({C=A * gpu_temp_var2})[[2]]
C_matMul_right<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  
  private_vector_size=GPUVar$private_vector_size
  vectorize_size=GPUVar$vectorSize
  
  defaultFloat=GPUVar$default_float
  defaultIndex=GPUVar$default_index_type
  defaultFloatV=paste0(defaultFloat,vectorize_size)
  
  
  privateVecLength=private_vector_size/getTypeSize(defaultFloat)
  
  
  
  #define macro for the matrix dimension
  dimMacroDef=c(
    paste0("gpu_A_row ",R_nrow(varInfo,rightVar1)),
    paste0("gpu_A_col ",R_ncol(varInfo,rightVar1)),
    paste0("gpu_B_row ",R_nrow(varInfo,rightVar2)),
    paste0("gpu_B_col ",R_ncol(varInfo,rightVar2)),
    paste0("gpu_vectorize_size ",vectorize_size),
    paste0("gpu_private_length (",privateVecLength,"/cl_local_thread_num)"),
    paste0("gpu_private_vectorize_length (gpu_private_length/gpu_vectorize_size)")
  )
  dimMacroDef=paste0("#define ",dimMacroDef)
  dimMacroUndef=c("gpu_A_row",
                  "gpu_A_col",
                  "gpu_B_row",
                  "gpu_B_col",
                  "gpu_vectorize_size",
                  "gpu_private_length",
                  "gpu_private_vectorize_length"
  )
  dimMacroUndef=paste0("#undef ",dimMacroUndef)
  
  
  supportVarDef=c(
    paste0(defaultIndex," gpu_group_worker_id=get_local_id(0);"),
    paste0(defaultFloat," gpu_private_spcae[gpu_private_length];"),
    paste0(defaultFloatV,"* gpu_private_vectorized_spcae=(",defaultFloatV,"*)gpu_private_spcae;"),
    paste0(defaultIndex," gpu_loopNum=ceil((",defaultFloat,")gpu_A_col/gpu_private_length);"),
    paste0(defaultIndex," gpu_start=0;"),
    paste0(defaultIndex," gpu_end=0;"),
    paste0(defaultIndex, " gpu_cur_length=0;")
  )
  
  #C=A%*%B
  #optimize the left matrix A
  A_opt_code=C_matMul_right_A(varInfo,Exp)
  #optimize the right matrix B
  B_opt_code=C_matMul_right_B(varInfo,Exp)
  
  A_row=R_nrow(varInfo,rightVar1)
  B_col=R_ncol(varInfo,rightVar2)
  if(isNumeric(A_row)&&A_row==1){
    mainCode=A_opt_code
  }else{
    if(isNumeric(B_col)&&B_col==1){
      mainCode=B_opt_code
    }else{
      mainCode=c(
        paste0("if(gpu_A_row>gpu_B_col){"),
        B_opt_code,
        "}else{",
        A_opt_code,
        "}"
      )
    }
  }
  
  
  
  code=c(
    "{",
    dimMacroDef,
    supportVarDef,
    mainCode,
    dimMacroUndef,
    "}"
  )
  code
}

#Exp=quote({C= gpu_temp_var2*B})[[2]]
#Store the row of A
C_matMul_right_A<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  rightInfo1=getVarInfo(varInfo,rightVar1)
  rightInfo2=getVarInfo(varInfo,rightVar2)
  
  
  vectorize_size=GPUVar$vectorSize
  defaultFloat=GPUVar$default_float
  defaultIndex=GPUVar$default_index_type
  defaultFloatV=paste0(defaultFloat,vectorize_size)
  
  
  #private assignment
  A_sub_private=R_expression_sub(varInfo,rightVar1,
                                     sub=c("gpu_i",paste0("gpu_start+gpu_k")),
                                     sub_C=TRUE,base=0,opt = list("gpu_i",c("gpu_k")))
  
  extCode_private_assign=finalizeExtCode(A_sub_private$extCode)
  
  private_assign_vector=c(
    "//Read a piece of row of A into the private memory",
    extCode_private_assign$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_private_length;gpu_k++){"),
    extCode_private_assign$L2,
    paste0("gpu_private_spcae[gpu_k]=",A_sub_private$value,";"),
    "}"
  )
  
  private_assign_scalar=c(
    "//Read a piece of row of A into the private memory",
    extCode_private_assign$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_cur_length;gpu_k++){"),
    extCode_private_assign$L2,
    paste0("gpu_private_spcae[gpu_k]=",A_sub_private$value,";"),
    "}"
  )
  
  
  
  #matrix multiplication in vector format
  #Data preparation
  B_vec=c()
  B_multi_sub_vector=list()
  B_multi_sub_vector$extCode=NULL
  #tmp_var=GPUVar$getTmpVar()
  for(i in seq_len(vectorize_size)){
    B_multi_sub_vector=R_expression_sub(varInfo,rightVar2,
                                        sub=c(paste0("gpu_k*gpu_vectorize_size+gpu_start+",i-1),"gpu_j"),
                                sub_C=c(TRUE,TRUE),base=0,opt=list("gpu_j",c("gpu_k")),extCode=B_multi_sub_vector$extCode)
    B_vec=c(B_vec,B_multi_sub_vector$value)
  }
  
  #matrix multiplication in vector format
  #Perform multiplication
  B_vector_code=paste0(defaultFloatV," gpu_B_vector=(",defaultFloatV,")(",
                       paste0(B_vec,collapse = ","),");")
  extCode_B_vec=finalizeExtCode(B_multi_sub_vector$extCode)
  
  #matrix multiplication in vector format
  matrixMultiVector=c(
    paste0(defaultFloatV," gpu_tmp=0;"),
    #paste0(defaultIndex," ",tmp_var,"=gpu_start;"),
    "//Matrix multiplication in vector form",
    extCode_B_vec$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_private_vectorize_length;gpu_k++){"),
    extCode_B_vec$L2,
    B_vector_code,
    paste0("gpu_tmp=fma(gpu_private_vectorized_spcae[gpu_k],gpu_B_vector,gpu_tmp);"),
    #paste0(tmp_var,"=",tmp_var,"+gpu_vectorize_size;"),
    "}"
  )
  
  
  #result writing back in vector format
  #temp vector Sum
  if(vectorize_size<=4){
    temp_vector_sum=paste0("dot(gpu_tmp,(",defaultFloatV,
                           ")(",paste0(rep(1,vectorize_size),collapse = ","),"));")
  }else{
    temp_vector_sum=paste0(paste0("gpu_tmp.s",0:(vectorize_size-1)),collapse = "+")
  }
  
  res_leftSub=R_expression_sub(varInfo,leftVar,
                               sub=c("gpu_i","gpu_j"),
                               sub_C=TRUE,base=0,opt = c("gpu_i","gpu_j"))
  extCode_res=finalizeExtCode(res_leftSub$extCode)
  
  writeBackRes_vector=c(
    "//Write the result back to the matrix",
    extCode_res$L2,
    "if(gpu_t==0){",
    paste0(res_leftSub$value,"=",temp_vector_sum,";"),
    "}else{",
    paste0(res_leftSub$value,"=",res_leftSub$value,"+",temp_vector_sum,";"),
    "}"
  )
  
  
  #matrix multiplication in scalar format
  #Data preparation
  B_multi_sub_scalar=R_expression_sub(varInfo,rightVar2,
                                      sub=c(paste0("gpu_start+gpu_k"),"gpu_j"),
                                      sub_C=TRUE,base=0,opt = list("gpu_j","gpu_k"))
  
  
  extCode_B_scalar=finalizeExtCode(B_multi_sub_scalar$extCode)
  
  matrixMultiScalar=c(
    "//In case that the rest values cannot form a vector",
    "//Element operation is used to compute the matrix multiplication",
    paste0(defaultFloat," gpu_tmp=0;"),
    extCode_B_scalar$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_cur_length;gpu_k++){"),
    extCode_B_scalar$L2,
    paste0("gpu_tmp=fma((",defaultFloat,")",B_multi_sub_scalar$value,",gpu_private_spcae[gpu_k],gpu_tmp);"),
    "}"
  )
  writeBackRes_scalar=c(
    "//Write the result back to the matrix",
    extCode_res$L2,
    "if(gpu_t==0){",
    paste0(res_leftSub$value,"=gpu_tmp;"),
    "}else{",
    paste0(res_leftSub$value,"=",res_leftSub$value,"+gpu_tmp;"),
    "}"
  )
  
  
  
  
  
  code=c(
    paste0("for(",defaultIndex," gpu_t=0;gpu_t<gpu_loopNum;gpu_t++){"),
    "gpu_start=gpu_end;",
    paste0("gpu_end=gpu_end+gpu_private_length;"),
    paste0("if(gpu_end>gpu_A_col) gpu_end=gpu_A_col;"),
    "gpu_cur_length=gpu_end-gpu_start;",
    extCode_private_assign$L0,
    extCode_res$L0,
    "if(gpu_cur_length==gpu_private_length){",
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    private_assign_vector,
    extCode_B_vec$L0,
    extCode_res$L1,
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    matrixMultiVector,
    writeBackRes_vector,
    "}",
    "}",
    "}else{",
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    private_assign_scalar,
    extCode_B_scalar$L0,
    extCode_res$L1,
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    matrixMultiScalar,
    writeBackRes_scalar,
    "}",
    "}",
    "}",
    "}"
  )
  code
}


#Store the col of B
C_matMul_right_B<-function(varInfo,Exp){
  
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  rightInfo1=getVarInfo(varInfo,rightVar1)
  rightInfo2=getVarInfo(varInfo,rightVar2)
  
  
  vectorize_size=GPUVar$vectorSize
  defaultFloat=GPUVar$default_float
  defaultIndex=GPUVar$default_index_type
  defaultFloatV=paste0(defaultFloat,vectorize_size)
  
  
  #private assignment
  B_sub_private=R_expression_sub(varInfo,rightVar2,
                                 sub=c("gpu_start+gpu_k",paste0("gpu_j")),
                                 sub_C=TRUE,base=0,opt = list("gpu_j",c("gpu_k")))
  
  extCode_private_assign=finalizeExtCode(B_sub_private$extCode)
  
  private_assign_vector=c(
    "//Read a column of B into the private memory",
    extCode_private_assign$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_private_length;gpu_k++){"),
    extCode_private_assign$L2,
    paste0("gpu_private_spcae[gpu_k]=",B_sub_private$value,";"),
    "}"
  )
  
  private_assign_scalar=c(
    "//Read a column of B into the private memory",
    extCode_private_assign$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_cur_length;gpu_k++){"),
    extCode_private_assign$L2,
    paste0("gpu_private_spcae[gpu_k]=",B_sub_private$value,";"),
    "}"
  )
  
  
  
  #matrix multiplication in vector format
  #Data preparation
  A_vec=c()
  A_multi_sub_vector=list()
  A_multi_sub_vector$extCode=NULL
  for(i in seq_len(vectorize_size)){
    A_multi_sub_vector=R_expression_sub(varInfo,rightVar1,
                                        sub=c("gpu_i",paste0("gpu_k*gpu_vectorize_size+gpu_start+",i-1)),
                                        sub_C=c(TRUE,TRUE),base=0,opt=list("gpu_i","gpu_k"),extCode=A_multi_sub_vector$extCode)
    A_vec=c(A_vec,A_multi_sub_vector$value)
  }
  
  #matrix multiplication in vector format
  #Perform multiplication
  A_vector_code=paste0(defaultFloatV," gpu_A_vector=(",defaultFloatV,")(",
                       paste0(A_vec,collapse = ","),");")
  extCode_A_vec=finalizeExtCode(A_multi_sub_vector$extCode)
  
  #matrix multiplication in vector format
  matrixMultiVector=c(
    paste0(defaultFloatV," gpu_tmp=0;"),
    "//Matrix multiplication in vector form",
    extCode_A_vec$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_private_vectorize_length;gpu_k++){"),
    extCode_A_vec$L2,
    A_vector_code,
    paste0("gpu_tmp=fma(gpu_private_vectorized_spcae[gpu_k],gpu_A_vector,gpu_tmp);"),
    "}"
  )
  
  
  #result writing back in vector format
  #temp vector Sum
  if(vectorize_size<=4){
    temp_vector_sum=paste0("dot(gpu_tmp,(",defaultFloatV,
                           ")(",paste0(rep(1,vectorize_size),collapse = ","),"));")
  }else{
    temp_vector_sum=paste0(paste0("gpu_tmp.s",0:(vectorize_size-1)),collapse = "+")
  }
  
  res_leftSub=R_expression_sub(varInfo,leftVar,
                               sub=c("gpu_i","gpu_j"),
                               sub_C=TRUE,base=0,opt = c("gpu_j","gpu_i"))
  extCode_res=finalizeExtCode(res_leftSub$extCode)
  
  writeBackRes_vector=c(
    "//Write the result back to the matrix",
    extCode_res$L2,
    "if(gpu_t==0){",
    paste0(res_leftSub$value,"=",temp_vector_sum,";"),
    "}else{",
    paste0(res_leftSub$value,"=",res_leftSub$value,"+",temp_vector_sum,";"),
    "}"
  )
  
  
  #matrix multiplication in scalar format
  #Data preparation
  A_multi_sub_scalar=R_expression_sub(varInfo,rightVar1,
                                      sub=c("gpu_i",paste0("gpu_start+gpu_k")),
                                      sub_C=TRUE,base=0,opt = list("gpu_i","gpu_k"))
  
  
  extCode_A_scalar=finalizeExtCode(A_multi_sub_scalar$extCode)
  
  matrixMultiScalar=c(
    "//In case that the rest values cannot form a vector",
    "//Element operation is used to compute the matrix multiplication",
    paste0(defaultFloat," gpu_tmp=0;"),
    extCode_A_scalar$L1,
    paste0("for(",defaultIndex," gpu_k=0;gpu_k<gpu_cur_length;gpu_k++){"),
    extCode_A_scalar$L2,
    paste0("gpu_tmp=fma((",defaultFloat,")",A_multi_sub_scalar$value,",gpu_private_spcae[gpu_k],gpu_tmp);"),
    "}"
  )
  writeBackRes_scalar=c(
    "//Write the result back to the matrix",
    extCode_res$L2,
    "if(gpu_t==0){",
    paste0(res_leftSub$value,"=gpu_tmp;"),
    "}else{",
    paste0(res_leftSub$value,"=",res_leftSub$value,"+gpu_tmp;"),
    "}"
  )
  
  
  
  
  
  code=c(
    paste0("for(",defaultIndex," gpu_t=0;gpu_t<gpu_loopNum;gpu_t++){"),
    "gpu_start=gpu_end;",
    paste0("gpu_end=gpu_end+gpu_private_length;"),
    paste0("if(gpu_end>gpu_A_col) gpu_end=gpu_A_col;"),
    "gpu_cur_length=gpu_end-gpu_start;",
    extCode_private_assign$L0,
    extCode_res$L0,
    "if(gpu_cur_length==gpu_private_length){",
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    private_assign_vector,
    extCode_A_vec$L0,
    extCode_res$L1,
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    matrixMultiVector,
    writeBackRes_vector,
    "}",
    "}",
    "}else{",
    paste0("for(",defaultIndex," gpu_j=0;gpu_j<gpu_B_col;gpu_j++){"),
    private_assign_scalar,
    extCode_A_scalar$L0,
    extCode_res$L1,
    paste0("for(",defaultIndex," gpu_i=0;gpu_i<gpu_A_row;gpu_i++){"),
    matrixMultiScalar,
    writeBackRes_scalar,
    "}",
    "}",
    "}",
    "}"
  )
  code
}







