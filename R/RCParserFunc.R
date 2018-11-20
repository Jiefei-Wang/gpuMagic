


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
C_subset<-function(varInfo,Exp){
  if(length(Exp)==1){
    curVar=deparse(Exp)
    if(is.numeric(Exp))
      return(curVar)
    curInfo=getVarInfo(varInfo,curVar,1)
    if(curInfo$dataType==T_matrix)
      return(paste0(curVar$address,"[0]"))
    if(curInfo$dataType==T_scale)
      return(paste0(curInfo$address))
  }
  if(Exp[[1]]=="["){
    if(length(Exp)==3){
      ExpChar=R_getVarSub(varInfo,Exp[[2]],C_subset(varInfo,Exp[[3]]),1)
      #ExpChar=paste0(Exp[[2]],"[(",GPUVar$default_index_type,")",C_subset(varInfo,Exp[[3]]),"-1]")
      return(ExpChar)
    }
    if(length(Exp==4)){
      #var_data=getVarInfo(varInfo,Exp[[2]])
      #var_ind=varInfo[[Exp[[2]]]]
      #size1=R_getVarSize1(varInfo,Exp[[2]])
      if(Exp[[3]]==""||Exp[[4]]=="")
        stop("Compilation error, please contact the author: ",deparse(Exp))
      #ExpChar=paste0(Exp[[2]],"[(",GPUVar$default_index_type,")",C_subset(varInfo,Exp[[3]]),
      #               "-1 +((",GPUVar$default_index_type,")",C_subset(varInfo,Exp[[4]]),"-1)*",size1,"]")
      ExpChar=R_getVarSub(varInfo,Exp[[2]],
                          C_subset(varInfo,Exp[[3]]),
                          C_subset(varInfo,Exp[[4]]))
      return(ExpChar)
    }
  }
  stop("The expression is not a subset function: ",deparse(Exp))
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
    code=c(paste0("printf(\"",varName,":\\n\");"),
           paste0("for(uint gpu_msg_i=0;gpu_msg_i<",size1,";gpu_msg_i++){"),
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
           " gpu_matMul_i=0;gpu_matMul_i<",R_nrow(varInfo,rightVar1),
           ";gpu_matMul_i++){"),
    paste0("for(",GPUVar$default_index_type,
           " gpu_matMul_i=0;gpu_matMul_i<",R_nrow(varInfo,rightVar1),
           ";gpu_matMul_i++){"),
    paste0(),
    paste0(),
    paste0(),
    paste0(),
    paste0(),
    paste0(),
    paste0()
    
  )
}











#Generate subset function
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

#Get an element from the matrix(eg. A[i,j])
#i,j is 1-based index
R_getVarSub<-function(varInfo,var,i,j=1){
  
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  address=curInfo$address
  
  if(j==1||j=="1"){
    return(paste0(address,"[(",GPUVar$default_index_type,")(",i,"-1)]"))
  }
  
  curInfo=getVarInfo(varInfo,var,varInfo$curVarVersion[[var]])
  size1=R_getVarSize1(varInfo,var)
  ifelse(curInfo$transpose,
         paste0(address,"[(",GPUVar$default_index_type,")(",j,
                "-1 +(",i,"-1)*",size1,")]"),
         paste0(address,"[(",GPUVar$default_index_type,")(",i,
                "-1 +(",j,"-1)*",size1,")]")
         )
  
}

R_nrow<-function(varInfo,var){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  ifelse(curInfo$transpose,
         R_getVarSize2(varInfo,var),
         R_getVarSize1(varInfo,var)
         )
}
R_ncol<-function(varInfo,var){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  ifelse(curInfo$transpose,
         R_getVarSize1(varInfo,var),
         R_getVarSize2(varInfo,var)
  )
}

R_getVarSize<-function(varInfo,var,ind){
  if(!is.character(var))
    var=deparse(var)
  curInfo=getVarInfo(varInfo,var,1)
  var_ind=varInfo$matrixInd[[var]]
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
R_getVarSize1<-function(varInfo,var){
  R_getVarSize(varInfo,var,1)
}
R_getVarSize2<-function(varInfo,var){
  R_getVarSize(varInfo,var,2)
}


