


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


#Generate subset function, used for matrix subsetting
#example B=A[sub1]
#sub:The index of the matrix, eg.sub1
#length: the dimension of the matrix, in case of the row index,it is nrow(A)
#name: the variable name to be used as the c variable index
#gpu_name__value_left and gpu_name_value will be used to index B[gpu_name__value_left] and A[gpu_name_value]
#The final result would be:
#for(gpu_name_value=something;gpu_name_value<something;gpu_name_value++){  <----res[[1]]
#gpu_name__value_left=f(gpu_name_value);                                   <----res[[2]]
#B[gpu_name__value_left] = A[gpu_name_value];
#}                                                                         <----res[[3]]

R_processSub<-function(varInfo,sub,length,name){
  if(isNumeric(sub)){
    forLoopStart=""
    subVal=c(paste0(GPUVar$default_index_type," gpu_",name,"_value=",sub,";"),
             paste0(GPUVar$default_index_type," gpu_",name,"_value_left=1;"))
    forLoopEnd=""
    return(list(forLoopStart,subVal,forLoopEnd))
  }
  if(sub==""){
    if(is.null(length))
      stop()
    forLoopStart=paste0("for(",GPUVar$default_index_type," gpu_",name,"_value=1;gpu_",name,"_value<=",length,";gpu_",name,"_value++){")
    subVal=paste0(GPUVar$default_index_type," gpu_",name,"_value_left=gpu_",name,"_value;")
    forLoopEnd="}"
    return(list(forLoopStart,subVal,forLoopEnd))
  }
  subInfo=getVarInfo(varInfo,sub,1)
  if(subInfo$dataType==T_scale){
    forLoopStart=""
    subVal=c(paste0(GPUVar$default_index_type," gpu_",name,"_value=",subInfo$address,";"),
             paste0(GPUVar$default_index_type," gpu_",name,"_value_left=1;"))
    forLoopEnd=""
    return(list(forLoopStart,subVal,forLoopEnd))
  }else{
    stop("Matrix subset by a vector is not supported")
  }
}

#get the i,jth element from the expression, 
#the expression can be a variable, a matrix subset or a number
#i,j can be interpreted as an R object or a C object, it is determined by i_C and j_C
R_expression_sub<-function(varInfo,Exp,i,j=1,opt=FALSE,i_C=FALSE,j_C=FALSE){
  if(Exp=="")
    return(i)
  if(is.character(Exp))
    Exp=parse(text=Exp)[[1]]
  #if the expression is an element
  if(length(Exp)==1){
    curVar=deparse(Exp)
    if(is.numeric(Exp))
      return(curVar)
    if(getVarProperty(varInfo,curVar,"lazyRef")){
      refExp=parse(text=getVarProperty(varInfo,curVar,"ref"))[[1]]
      ref_i=deparse(refExp[[3]])
      ref_j=deparse(refExp[[4]])
      ref_i_C=R_expression_sub(varInfo,ref_i,i,1,opt=FALSE,i_C=i_C,FALSE)
      ref_j_C=R_expression_sub(varInfo,ref_j,j,1,opt=FALSE,j_C=j_C,FALSE)
      res=R_expression_sub(varInfo,refExp[[2]],ref_i_C,ref_j_C,opt=FALSE,i_C=TRUE,j_C=TRUE)
      return(res)
    }
    dataType=getVarProperty(varInfo,curVar,"dataType")
    if(dataType==T_scale)
      return(getVarProperty(varInfo,curVar,"address",1))
    if(dataType==T_matrix){
      if(!i_C){
        sub1=parse(text=i)[[1]]
        i_C_ind=R_expression_sub(varInfo,sub1,1,1,opt=FALSE,TRUE,TRUE)
      }else{
        i_C_ind=i
      }
      if(!j_C){
        sub2=parse(text=j)[[1]]
        j_C_ind=R_expression_sub(varInfo,sub2,1,1,opt=FALSE,TRUE,TRUE)
      }else{
        j_C_ind=j
      }
      res=R_getVarSub(varInfo,Exp,i_C_ind,j_C_ind,opt)
      return(res)
    }
    stop("unrecognized code: ",deparse(Exp))
  }
  if(Exp[[1]]=="["){
    curVar=Exp[[2]]
    if(Exp[[3]]=="")
      sub1=""
    else
      sub1=Exp[[3]]
    
    if(length(Exp)==3){
      sub2=1
    }else{
      if(Exp[[4]]=="")
        sub2=""
      else
        sub2=Exp[[4]]
    }
    i_C_ind=R_expression_sub(varInfo,sub1,i,1,opt=FALSE,i_C,TRUE)
    j_C_ind=R_expression_sub(varInfo,sub2,j,1,opt=FALSE,j_C,TRUE)
    res=R_getVarSub(varInfo,curVar,i_C_ind,j_C_ind,opt)
    return(res)
  }
  stop("unrecognized code: ",deparse(Exp))
}


#Get an element from the matrix(eg. A[i,j]), the transpose will be taken into account
#i,j is 1-based index
#i,j should be either a number or a variable in C code
R_getVarSub<-function(varInfo,var,i,j=1,opt=FALSE){
  
  if(!is.character(var))
    var=deparse(var)
  
  if(var=="")
    return(as.character(i))
  
  if(isNumeric(i))
    sub1=Simplify(paste0(i,"-1"))
  else
    sub1=paste0(i,"-1")
  
  address=getVarProperty(varInfo,var,"address",1)
  if(j==1||j=="1"){
    return(paste0(address,"[(",GPUVar$default_index_type,")(",sub1,")]"))
  }
  
  transpose=getVarProperty(varInfo,var,"transpose")
  size1=R_getVarSize1(varInfo,var)
  if(isNumeric(j))
    sub2=Simplify(paste0(j,"-1"))
  else
    sub2=paste0(j,"-1")
  
  if(transpose){
    sub=paste0(sub2," +(",sub1,")*",size1)
    res=paste0(address,"[(",GPUVar$default_index_type,")(",sub,")]")
  }else{
    sub=paste0(sub1," +(",sub2,")*",size1)
    res=paste0(address,"[(",GPUVar$default_index_type,")(",sub,")]")
  }
  return(res)
}

#Get the number of rows for a matrix in C format
R_nrow<-function(varInfo,var){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  ifelse(curInfo$transpose,
         R_getVarSize2(varInfo,var),
         R_getVarSize1(varInfo,var)
         )
}
#Get the number of rows for a matrix in C format
R_ncol<-function(varInfo,var){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  ifelse(curInfo$transpose,
         R_getVarSize1(varInfo,var),
         R_getVarSize2(varInfo,var)
  )
}
R_length<-function(varInfo,var){
  return(Simplify(paste0(R_getVarSize1(varInfo,var),"*",
                R_getVarSize2(varInfo,var))))
  
}


R_getVarSize<-function(varInfo,var,ind){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  if(curInfo$dataType==T_scale) return(1)
  if(curInfo$lazyRef){
    refCode=parse(text=curInfo$ref)[[1]]
    refVar=refCode[[2]]
    if(ind==1){
      if(refCode[[3]]=="")
        return(R_nrow(varInfo,refVar))
      
      sub1=refCode[[3]]
      if(isNumeric(sub1))
        return(1)
      
      return(R_length(varInfo,sub1))
    }
    if(ind==2){
      if(refCode[[4]]=="")
        return(R_ncol(varInfo,refVar))
      
      sub2=refCode[[4]]
      if(isNumeric(sub2))
        return(1)
      
      return(R_length(varInfo,sub2))
    }
  }
  
  var_ind=varInfo$matrixInd[[var]]
  if(var_ind==""||is.na(var_ind))
    stop("Error in finding the matrix size") 
  loc=NA
  if(curInfo$location=="global"&&!curInfo$shared)
    loc="global_private_"
  if(curInfo$location=="global"&&curInfo$shared)
    loc="global_shared_"
  if(curInfo$location=="local"&&!curInfo$shared)
    loc="local_private_"
  if(curInfo$location=="local"&&curInfo$shared)
    loc="local_shared_"
  if(is.na(loc))
    stop("undetermined matrix property!")
  
    
  size=paste0(GPUVar[[paste0(loc,"size",ind)]],"[",var_ind,"]")
  
  size
}
#get the variable row number in C code format
#Use R_nrow instead, this function does not take the transpose into account
R_getVarSize1<-function(varInfo,var){
  R_getVarSize(varInfo,var,1)
}
#get the variable column number in C code format
#Use R_ncol instead, this function does not take the transpose into account
R_getVarSize2<-function(varInfo,var){
  R_getVarSize(varInfo,var,2)
}


