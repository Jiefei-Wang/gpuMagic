
#Generate subset function, used for matrix subsetting
#example B=A[sub1]
#sub:The index of the matrix, eg.sub1
#length: the dimension of the matrix, in case of the row index,it is nrow(A)
#name: the variable name to be used as the c variable index
#gpu_name__value_left and gpu_name_value will be used to index B[gpu_name__value_left] and A[gpu_name_value]
#The final result would be:
#for(gpu_name_value=something;gpu_name_value<something;gpu_name_value++){  <----res[[1]]
#gpu_name_value_left=f(gpu_name_value);                                   <----res[[2]]
#B[gpu_name_value_left] = A[gpu_name_value];
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

#Expression should be a variable
R_oneIndex_exp_sub<-function(varInfo,Exp,k,k_C=FALSE){
  if(k_C){
    k_C_ind=k
  }else{
    k_C_ind=R_expression_sub(varInfo,k,1)
  }
  
  i_C_ind=paste0(k_C_ind,"-",R_nrow(varInfo,Exp),"*gpu_oneSub_tmp")
  j_C_ind="gpu_oneSub_tmp+1"
  
  
    res=list(
      tmpVar=paste0(GPUVar$default_index_type," gpu_oneSub_tmp=floor((",k_C_ind,"-1)/",R_nrow(varInfo,Exp),");"),
      result=R_expression_sub(varInfo,Exp,i=i_C_ind,j=j_C_ind,i_C=TRUE,j_C=TRUE)
      )
   return(res)
}


#get the i,jth element from the expression, 
#the expression can be a variable, a matrix subset or a number
#i,j can be interpreted as an R object or a C object, it is determined by i_C and j_C
#Special case:
#Exp can be empty, then i will be returned
#Exp can be a value, then the value will be returned
#Exp can be a scaler, then the value will be returned
R_expression_sub<-function(varInfo,Exp,i,j=1,opt=FALSE,optCode=list(),i_C=FALSE,j_C=FALSE,base=1){
  
  i=paste0(i,"+",1-base)
  j=paste0(j,"+",1-base)
  base=1
  
  if(Exp==""){
      if(i_C)
        return(i)
      else
        return(Simplify(i))
  }
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
      
      ref_i_C=R_expression_sub(varInfo,ref_i,i=i,j=base,i_C=i_C)
      ref_j_C=R_expression_sub(varInfo,ref_j,i=j,j=base,j_C=j_C)
      res=R_expression_sub(varInfo,refExp[[2]],i=ref_i_C,j=ref_j_C,i_C=TRUE,j_C=TRUE)
      return(res)
    }
    dataType=getVarProperty(varInfo,curVar,"dataType")
    if(dataType==T_scale)
      return(getVarProperty(varInfo,curVar,"address",1))
    if(dataType==T_matrix){
      if(!i_C){
        sub1=parse(text=i)[[1]]
        i_C_ind=R_expression_sub(varInfo,sub1,i=1,i_C=TRUE)
      }else{
        i_C_ind=i
      }
      if(!j_C){
        sub2=parse(text=j)[[1]]
        j_C_ind=R_expression_sub(varInfo,sub2,i=1,i_C=TRUE)
      }else{
        j_C_ind=j
      }
      res=R_getVarSub(varInfo,Exp,i_C_ind,j_C_ind,opt,optCode)
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
    i_C_ind=R_expression_sub(varInfo,sub1,i=i,i_C=i_C)
    j_C_ind=R_expression_sub(varInfo,sub2,i=j,i_C=j_C)
    res=R_getVarSub(varInfo,curVar,i_C_ind,j_C_ind,opt,optCode)
    return(res)
  }
  stop("unrecognized code: ",deparse(Exp))
}


#Get an element from the matrix(eg. A[i,j]), the transpose will be taken into account
#i,j is 1-based index by default
#i,j should be either a number or a variable in C code
#If opt=FALSE, the c code will be returned
#If pt=TRUE, a list will be returned. list element: C_sub, rowOffset, colOffset
#optCode should be a list with rowVar,and colVar as the elements. Both element can be optional
R_getVarSub<-function(varInfo,var,i,j=1,opt=FALSE,optCode=list(),base=1){
  
  if(!is.character(var))
    var=deparse(var)
  
  if(var=="")
    return(as.character(i))
  
  address=getVarProperty(varInfo,var,"address",1)
  transpose=getVarProperty(varInfo,var,"transpose")
  
  #Get the simplified index
  if(isNumeric(i)){
    sub1=Simplify(paste0(i,"-",base))
  }else{
    sub1=paste0(i,"-",base)
  }
  
  if(isNumeric(j)){
    sub2=Simplify(paste0(j,"-",base))
  }else{
    sub2=paste0(j,"-",base)
  }
  
  #compute the matrix offset
  size1=R_getVarSize1(varInfo,var)
  if(transpose){
    if(sub1==0)
      rowOffset=0
    else
      rowOffset=paste0("(",sub1,")*",size1)
    colOffset=sub2
  }else{
    rowOffset=sub1
    if(sub2==0)
      colOffset=0
    else
      colOffset=paste0("(",sub2,")*",size1)
  }
  
  if(!is.null(optCode[["rowVar"]]))
    rowOffset=optCode[["rowVar"]]
  if(!is.null(optCode[["colVar"]]))
    colOffset=optCode[["colVar"]]
  
  offset=paste0(rowOffset,"+",colOffset)
  c_sub=R_C_Sub(address,offset)
  
  
  if(opt)
    return(list(c_sub=c_sub,rowOffset=rowOffset,colOffset=colOffset))
  else
    return(c_sub)
}
#The C subset: var[offset]
#All the argument should be available in C code
R_C_Sub<-function(var,offset){
  paste0(var,"[(",GPUVar$default_index_type,")(",offset,")]")
}

#Get the number of rows for a matrix in C format
R_nrow<-function(varInfo,var){
  if(!is.character(var))
    var=deparse(var)
  if(isNumeric(var))
    return(1)
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
  if(isNumeric(var))
    return(1)
  curInfo=getVarInfo(varInfo,var,1)
  ifelse(curInfo$transpose,
         R_getVarSize1(varInfo,var),
         R_getVarSize2(varInfo,var)
  )
}
R_length<-function(varInfo,var){
  return(Simplify(paste0(R_nrow(varInfo,var),"*",
                         R_ncol(varInfo,var))))
  
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
