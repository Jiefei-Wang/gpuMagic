


C_call_assign<-function(varInfo,Exp){
  leftExp=Exp[[2]]
  rightExp=Exp[[3]]
  
  
  if(is.call(leftExp)){
    func_char=deparse(leftExp[[1]])
    func=.cFuncs[[func_char]]
    if(!is.null(func)){
      C_leftExp=func(varInfo,leftExp)
    }else{
      stop("Unsupported left expression: ",deparse(Exp))
    }
  }else{
    C_leftExp=deparse(leftExp)
  }
  
  if(is.call(rightExp)){
    func_char=deparse(rightExp[[1]])
    func=.cFuncs[[func_char]]
    if(!is.null(func)){
      C_rightExp=func(varInfo,rightExp)
    }else{
      stop("Unsupported right expression: ",deparse(Exp))
    }
    
  }else{
    C_rightExp=deparse(rightExp)
  }
  
  if(C_rightExp==""){
    if(deparse(Exp[[1]])=="==")
      stop("Unexpected expression: ",deparse(Exp))
    return("")
  }
  if(C_leftExp==""){
    stop("Unexpected expression: ",deparse(Exp))
  }
  
  
  curCode=paste0(C_leftExp,deparse(Exp[[1]]),C_rightExp,";")
  return(curCode)
  
}

C_arithmaticOP<-function(varInfo,Exp){
  leftEle=C_subset(varInfo,Exp[[2]])
  rightEle=C_subset(varInfo,Exp[[3]])
  op=deparse(Exp[[1]])
  if(op!="/")
    code=paste0(leftEle,op,rightEle)
  else
    code=paste0("(",gpuMagic.option$getDefaultFloat(),")",leftEle,op,rightEle)
  code
}

C_length<-function(varInfo,Exp){
  return(paste0(R_getVarSize1(varInfo,Exp[[2]]),"*",R_getVarSize2(varInfo,Exp[[2]])))
}
C_nrow<-function(varInfo,Exp){
  return(R_getVarSize1(varInfo,Exp[[2]]))
}
C_ncol<-function(varInfo,Exp){
  return(R_getVarSize2(varInfo,Exp[[2]]))
}
#This function assume the result of the subset is a number
C_subset<-function(varInfo,Exp){
  if(length(Exp)==1)
    return(R_expression_sub(varInfo,Exp,1))
  if(length(Exp)==3){
    var=Exp[[2]]
    sub1=Exp[[3]]
    sub2=1
    return(R_expression_sub(varInfo,var,deparse(sub1),sub2))
  }else{
    var=Exp[[2]]
    sub1=Exp[[3]]
    sub2=Exp[[4]]
    return(R_expression_sub(varInfo,var,deparse(sub1),sub2))
  }
}
C_floor<-function(varInfo,Exp){
  code=paste0("floor(",deparse(Exp[[2]]),")")
  return(code)
}
C_ceil<-function(varInfo,Exp){
  code=paste0("ceil(",deparse(Exp[[2]]),")")
  return(code)
}
C_return<-function(varInfo,Exp){
  returnVar=Exp[[2]]
  
  curCode=c(
          paste0(GPUVar$default_index_type," gpu_return_k=0;"),
          paste0("for(",GPUVar$default_index_type," gpu_return_i=1;gpu_return_i<=",R_nrow(varInfo,returnVar),";gpu_return_i++){\n"),
          paste0("for(",GPUVar$default_index_type," gpu_return_j=1;gpu_return_j<=",R_ncol(varInfo,returnVar),";gpu_return_j++){\n"),
          paste0(GPUVar$return_variable,"[gpu_return_k+",GPUVar$gpu_global_id,"*",GPUVar$return_size,"]=",R_expression_sub(varInfo,returnVar,"gpu_return_i","gpu_return_j",i_C =T,j_C=T),";"),
          "gpu_return_k=gpu_return_k+1;",
          "}",
          "}"
          )
  curCode
  
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
    printType="%d"}
  if(curInfo$dataType==T_scale){
    code=paste0("printf(\"",varName,": ",printType,"\\n\",",curInfo$address,")")
    return(code)
  }
  if(curInfo$dataType==T_matrix){
    size1=R_getVarSize1(varInfo,varName)
    size2=R_getVarSize2(varInfo,varName)
    subsetCode=paste0(curInfo$address,"[gpu_msg_i +gpu_msg_j*",size1,"]")
    code=c(paste0("for(uint gpu_msg_i=0;gpu_msg_i<",size1,";gpu_msg_i++){"),
           paste0("for(uint gpu_msg_j=0;gpu_msg_j<",size2,";gpu_msg_j++){"),
           paste0("printf(\"",printType,"  \",",subsetCode,");"),
           "}",
           "printf(\"\\n\");",
           "}"
    )
    
  }
}

C_setVersion<-function(varInfo,Exp){
  varName=deparse(Exp[[2]])
  version=as.numeric(Exp[[3]])
  varInfo$curVarVersion[[varName]]=version
  return("")
}

C_oneSub<-function(varInfo,Exp){
  leftExp=deparse(Exp[[2]])
  rightExp=Exp[[3]]
  
  rightSub1=deparse(rightExp[[3]])
  
  if(isNumeric(rightSub1)){
    rightSubType1=T_scale
    rightSubVal1=rightSub1
  }else{
    if(rightSub1!=""){
      rightSubInfo1=getVarInfo(varInfo,rightSub1,1)
      rightSubType1=rightSubInfo1$dataType
      rightSubVal1=rightSubInfo1$address
    }else{
      rightSubType1=T_matrix
    }
  }
  
  
  if(rightSubType1==T_scale){
    code=paste0(leftExp,"=",R_getVarSub(varInfo,rightExp[[2]],rightSubVal1,1),";")
    return(code)
  }
  stop()
  subRes1=R_processSub(varInfo,rightSub1,R_nrow(varInfo,rightExp[[2]]),"index")
  code=c(subRes1[[1]],subRes1[[2]],
         subRes2[[1]],subRes2[[2]],
         paste0(R_getVarSub(varInfo,leftExp,"gpu_row_value_left",1),"="
                ,R_getVarSub(varInfo,rightExp[[2]],"gpu_index_value",1),";"),
         subRes1[[3]],subRes2[[3]]
  )
  
  code[code!=""]
  
}
C_twoSub<-function(varInfo,Exp){
  leftExp=deparse(Exp[[2]])
  rightExp=Exp[[3]]
  
  rightSub1=deparse(rightExp[[3]])
  rightSub2=deparse(rightExp[[4]])
  
  if(isNumeric(rightSub1)){
    rightSubType1=T_scale
    rightSubVal1=rightSub1
  }else{
    if(rightSub1!=""){
      rightSubInfo1=getVarInfo(varInfo,rightSub1,1)
      rightSubType1=rightSubInfo1$dataType
      rightSubVal1=rightSubInfo1$address
    }else{
      rightSubType1=T_matrix
    }
  }
  
  if(isNumeric(rightSub2)){
    rightSubType2=T_scale
    rightSubVal2=rightSub1
  }else{
    if(rightSub2!=""){
      rightSubInfo2=getVarInfo(varInfo,rightSub2,1)
      rightSubType2=rightSubInfo2$dataType
      rightSubVal2=rightSubInfo2$address
    }else{
      rightSubType2=T_matrix
    }
  }
  
  if(rightSubType1==T_scale&&rightSubType2==T_scale){
    code=paste0(leftExp,"=",R_getVarSub(varInfo,rightExp[[2]],rightSubVal1,rightSubVal2),";")
    return(code)
  }
  
  #paste0("for(",GPUVar$default_index_type," gpu_sub_i=1;gpu_sub_i<=1;gpu_sub_i++){")
  subRes1=R_processSub(varInfo,rightSub1,R_nrow(varInfo,rightExp[[2]]),"row")
  subRes2=R_processSub(varInfo,rightSub2,R_ncol(varInfo,rightExp[[2]]),"col")
  code=c(subRes1[[1]],subRes1[[2]],
         subRes2[[1]],subRes2[[2]],
         paste0(R_getVarSub(varInfo,leftExp,"gpu_row_value_left","gpu_col_value_left"),"="
                ,R_getVarSub(varInfo,rightExp[[2]],"gpu_row_value","gpu_col_value"),";"),
         subRes1[[3]],subRes2[[3]]
  )
  
  code[code!=""]
}


C_matMul<-function(varInfo,Exp){
  leftVar=Exp[[2]]
  rightExp=Exp[[3]]
  rightVar1=rightExp[[2]]
  rightVar2=rightExp[[3]]
  
  code=c(
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
    "}"
  )
  code
}



