#Fill the target vector with default value to reach the desinged length
fillDefauleValue<-function(target,designLen,default){
  if(designLen==length(target)){
    return(target)
  }
  if(designLen>length(target)){
    target_new=rep(default,designLen)
    target_new[seq_len(designLen)]=target
  }else{
    target_new=target[seq_len(designLen)]
  }
  target_new
}
#R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),c(3,6),opt=c(F,T))
#R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),3,opt=F)
R_expression_sub<-function(varInfo,Exp,sub,sub_C=FALSE,opt=FALSE,extCode=NULL,base=1){
  #fill the vector with the first element to make the length of it consistant with the length of Exp
  sub_C=fillDefauleValue(sub_C,length(sub),sub_C[1])
  if(is.logical(opt[1]))
    opt=fillDefauleValue(opt,length(sub),opt[1])
  
  if(length(sub)==1){
    res=R_oneIndex_exp_sub(varInfo,Exp,sub,sub_C,opt,extCode,base)
  }else{
    res=R_twoIndex_exp_sub(varInfo,Exp,sub[1],sub[2],sub_C[1],sub_C[2],opt,extCode,base)
  }
  return(res)
}


#Expression should be a variable or a matrix subset
#R_oneIndex_exp_sub(varInfo,quote(A[tmp1]),3)
R_oneIndex_exp_sub<-function(varInfo,Exp,k,k_C=FALSE,opt=FALSE,extCode=NULL,base=1){
  k=paste0(k,"+",1-base)
  k=CSimplify(k,k_C)
  
  
  
  #If the expression is empty, return the k index.
  if(Exp==""){
    k_C_ind=wrapIndex(varInfo,k,k_C,extCode)
    return(list(value=k_C_ind$value,extCode=k_C_ind$extCode))
  }
  #Simplify the expression to make sure no 1+0 cases
  Exp=Simplify(Exp)
  #Check if the result is a number
  if(isNumeric(Exp)){
    return(list(value=toCharacter(Exp),extCode=extCode))
  }
  #Convert the character to the expression
  Exp=toExpression(Exp)
  
  #If the expression is a variable
  if(is.symbol(Exp)){
    k_C_ind=wrapIndex(varInfo,k,k_C,extCode)
    res=oneIndex_to_twoIndex(varInfo,Exp,k_C_ind$value,rowNum=R_nrow(varInfo,Exp)
                              ,opt=opt,extCode=k_C_ind$extCode)
    return(res)
  }
  #if the expression is a subset of a matrix
  if(Exp[[1]]=="["){
    curVar=Exp[[2]]
    args=matchBracketFunc(Exp)
    #if it is one index subset
    if(is.null(args$j)){
      sub=args$i
      k_C_ind=R_oneIndex_exp_sub(varInfo,sub,k=k,k_C=k_C,extCode=extCode)
      res=R_oneIndex_exp_sub(varInfo,curVar,k=k_C_ind$value,k_C=TRUE,opt=opt,extCode=extCode)
      return(res)
    }else{
      size=R_nrow(varInfo,Exp)
      k_C_ind=wrapIndex(varInfo,k,k_C,extCode)
      res=oneIndex_to_twoIndex(varInfo,Exp,k_C_ind$value,rowNum=size
                                ,opt=opt,extCode=k_C_ind$extCode)
      return(res)
      
    }
  }
  stop("unrecognized code: ",deparse(Exp))
}
#assume 1-based index
oneIndex_to_twoIndex<-function(varInfo,Exp,k_C_value,rowNum,opt=opt,extCode=NULL){
  tmpVar=GPUVar$getTmpVar()
  tmpVar_value=CSimplify(paste0("(",GPUVar$default_index_type,")((",k_C_value,"-1)/(",rowNum,"))"))
  #if the temporary variable is a constant, it will be plug into the code
  #Otherwise, check if the code can use existing extra code
  if(isNumeric(tmpVar_value)){
    tmpVar=tmpVar_value
  }else{
    if(isVarExist(extCode,GPUVar$default_index_type,tmpVar_value,order=0)){
      res=getVarFromExtCode(extCode,GPUVar$default_index_type,tmpVar_value,order=0)
      tmpVar=res$var
      extCode=res$extCode
    }
  }
  
  i_C_ind=paste0(k_C_value,"-",rowNum,"*",tmpVar)
  j_C_ind=paste0(tmpVar,"+1")
  
  res=R_twoIndex_exp_sub(varInfo,Exp,i=i_C_ind,j=j_C_ind,i_C=TRUE,j_C=TRUE,opt=opt,extCode=extCode)
  #Check if the temporary variable is still in the result,
  #if yes, add it into extra code if the extra code don't have it,
  #if not, do not add it.
  if(!isNumeric(tmpVar_value)&&length(grep(tmpVar,res$value,fixed=T))!=0){
    if(!isVarExist(extCode,GPUVar$default_index_type,tmpVar_value,order=0)){
      res$extCode=addVarDefInExt(res$extCode,GPUVar$default_index_type,tmpVar,tmpVar_value,order=0)
    }
  }
  return(res)
  
}


#get the i,jth element from the expression, 
#the expression can be a variable, a matrix subset or a number
#i,j can be interpreted as an R object or a C object, it is determined by i_C and j_C
#Special case:
#Exp can be empty, then i will be returned
#Exp can be a value, then the value will be returned
#Exp can be a scalar, then the value will be returned
#R_twoIndex_exp_sub(varInfo,quote(gpu_temp_var2[C,]),3,6)
R_twoIndex_exp_sub<-function(varInfo,Exp,i,j=1,i_C=FALSE,j_C=FALSE,opt=FALSE,extCode=NULL,base=1){
  opt=fillDefauleValue(opt,2,opt[1])
  #Convert all the 0-based index to 1-based index
  i=paste0(i,"+",1-base)
  j=paste0(j,"+",1-base)
  base=1
  i=CSimplify(i,i_C)
  j=CSimplify(j,j_C)
  
  #If the expression is empty, return the i index.
  if(Exp==""){
    if(isNumeric(j)&&as.character(j)!="1")
      stop("Incorrect subset index")
    
    i_C_ind=wrapIndex(varInfo,i,i_C,extCode)
    return(list(value=i_C_ind$value,extCode=i_C_ind$extCode))
  }
  #Simplify the expression to make sure no 1+0 cases
  Exp=Simplify(Exp)
  #Check if the result is a number
  if(isNumeric(Exp)){
    return(list(value=toCharacter(Exp),extCode=extCode))
  }
  #Convert the character to the expression
  Exp=toExpression(Exp)
  
  #if the expression contains only one element
  if(length(Exp)==1){
    curVar=deparse(Exp)
    curInfo=getVarInfo(varInfo,curVar)
    #If the expression is a lazy reference
    if(curInfo$isRef){
      refExp=parse(text=curInfo$specialContent)[[1]]
      if(curInfo$transpose){
        if(is.logical(opt[1]))
          opt=opt[2:1]
        res=R_twoIndex_exp_sub(varInfo,refExp,j,i,i_C=j_C,j_C=i_C,opt=opt,extCode=extCode,base=base)
      }else{
        res=R_twoIndex_exp_sub(varInfo,refExp,i,j,i_C=i_C,j_C=j_C,opt=opt,extCode=extCode,base=base)
      }
      return(res)
    }
    
    if(curInfo$isSeq){
      seqExp=parse(text=getVarProperty(varInfo,curVar,"specialContent"))[[1]]
      
      seqInfo=getSeqAddress(varInfo,curVar)
      
      i_C_ind=wrapIndex(varInfo,i,i_C,extCode)
      res=list()
      res$extCode=i_C_ind$extCode
      res$value=Simplify(paste0(seqInfo$from,"+","(",i_C_ind$value,"-1)*",seqInfo$by))
      return(res)
    }
    #If the expression is just a variable
    dataType=curInfo$dataType
    #Scalar
    if(dataType==T_scale)
      return(list(value=curInfo$address,extCode=extCode))
    #Matrix
    if(dataType==T_matrix){
      
      i_C_ind=wrapIndex(varInfo,i,i_C,extCode)
      j_C_ind=wrapIndex(varInfo,j,j_C,i_C_ind$extCode)
      res=R_getVarSub(varInfo,Exp,i_C_ind$value,j_C_ind$value,opt=opt,extCode=j_C_ind$extCode)
      return(res)
    }
    stop("unrecognized code: ",deparse(Exp))
  }
  #If the expression is also a subset of a matrix
  if(Exp[[1]]=="["){
    curVar=Exp[[2]]
    args=matchBracketFunc(Exp)
    if(is.null(args$j)){
      ref_k=args$i
      ref_k_C=R_oneIndex_exp_sub(varInfo,ref_k,k=i,k_C=i_C,extCode=extCode)
      res=R_oneIndex_exp_sub(varInfo,curVar,k=ref_k_C$value,k_C=TRUE,opt=opt,extCode=ref_k_C$extCode)
    }else{
      ref_i=args$i
      ref_j=args$j
      ref_i_C=R_oneIndex_exp_sub(varInfo,ref_i,k=i,k_C=i_C,extCode=extCode)
      ref_j_C=R_oneIndex_exp_sub(varInfo,ref_j,k=j,k_C=j_C,extCode=ref_i_C$extCode)
      res=R_twoIndex_exp_sub(varInfo,curVar,i=ref_i_C$value,j=ref_j_C$value,
                           i_C=TRUE,j_C=TRUE,
                           opt=opt,extCode=ref_j_C$extCode)
    }
    return(res)
  }
  stop("unrecognized code: ",deparse(Exp))
}
#Return the first element in the argument i
#If the index is a c variale, direct return it as a list
#If the index is an R variable, use subset code to process it
wrapIndex<-function(varInfo,i,i_C,extCode){
  if(!i_C){
    res=R_twoIndex_exp_sub(varInfo,Exp=i,i=1,j=1,extCode=extCode)
  }else{
    res=list(value=i,extCode=extCode)
  }
  res
}

#Get an element from the matrix(eg. A[i,j]), the transpose will be taken into account
#i,j is 1-based index by default
#i,j should be either a number or a variable in C code
#If opt=FALSE, the c code will be returned
#If pt=TRUE, a list will be returned. list element: C_sub, rowOffset, colOffset
#optCode should be a list with rowVar,and colVar as the elements. Both element can be optional
R_getVarSub<-function(varInfo,var,i,j=1,opt=c(FALSE,FALSE),extCode=NULL,base=1){
    var=toCharacter(var)
  
  if(var=="")
    stop("something is wrong..")
    #return(list(value=toCharacter(i),extCode=extCode))
  
  curInfo=getVarInfo(varInfo,var)
  address=curInfo$address
  transpose=curInfo$transpose
  
  
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
    tmp=sub1
    sub1=sub2
    sub2=tmp
    if(is.logical(opt[1]))
    opt=opt[2:1]
  }
  
  rowOffset=sub1
  colOffset=paste0("(",sub2,")*",size1)
  
  
  rowOffset=CSimplify(rowOffset,TRUE)
  colOffset=CSimplify(colOffset,TRUE)
  
  if(is.logical(opt[1])&&opt[1]&&!isSymbol(rowOffset)){
    res=getVarFromExtCode(extCode,GPUVar$default_index_type,rowOffset)
    extCode=res$extCode
    rowOffset=res$var
  }
  if(is.logical(opt[2])&&opt[2]&&!isSymbol(rowOffset)){
    res=getVarFromExtCode(extCode,GPUVar$default_index_type,colOffset)
    extCode=res$extCode
    colOffset=res$var
  }
  
  offset=paste0(rowOffset,"+",colOffset)
  c_sub=R_C_Sub(address,offset,simplification=TRUE)
  
  return(list(value=c_sub,extCode=extCode))
}

#The C subset: var[offset]
#All the argument should be available in C code
R_C_Sub<-function(var,offset,simplification=FALSE){
  res=paste0(var,"[(",GPUVar$default_index_type,")(",offset,")]")
  if(simplification)
    res=CSimplify(res,TRUE)
  return(res)
}

#Get the number of rows for a matrix in C format
R_nrow<-function(varInfo,var){
  var_char=toCharacter(var)
  var=toExpression(var)
  if(isNumeric(var_char))
    return(1)
  if(is.call(var))
    return(R_getVarSize1(varInfo,var))
  
  curInfo=getVarInfo(varInfo,var)
  ifelse(curInfo$transpose,
         R_getVarSize2(varInfo,var),
         R_getVarSize1(varInfo,var)
  )
}
#Get the number of rows for a matrix in C format
R_ncol<-function(varInfo,var){
  var_char=toCharacter(var)
  var=toExpression(var)
  if(isNumeric(var_char))
    return(1)
  if(is.call(var))
    return(R_getVarSize2(varInfo,var))
  
  curInfo=getVarInfo(varInfo,var)
  ifelse(curInfo$transpose,
         R_getVarSize1(varInfo,var),
         R_getVarSize2(varInfo,var)
  )
}
R_length<-function(varInfo,var){
  return(CSimplify(paste0(R_nrow(varInfo,var),"*",
                         R_ncol(varInfo,var))))
  
}


R_getVarSize<-function(varInfo,var,ind){
  var_char=toCharacter(var)
  var=toExpression(var)
  
  #Detect if the variable is a subset of a matrix
  #Or a lazy reference
  Exp=NULL
  if(is.call(var)&&var[[1]]=="["){
    Exp=var
  }else{
    curInfo=getVarInfo(varInfo,var)
    if(curInfo$isRef){
      Exp=parse(text=curInfo$specialContent)[[1]]
    }
    
  }
  #If the variable is a subset of a matrix
  if(!is.null(Exp)){
    args=matchBracketFunc(Exp)
    refVar=Exp[[2]]
    if(ind==1){
      #If the first index is empty. eg: A[,1]
      if(!is.null(args$i)&&args$i=="")
        return(R_nrow(varInfo,refVar))
      
      sub1=args$i
      if(isNumeric(sub1))
        return(1)
      
      return(R_length(varInfo,sub1))
    }
    if(ind==2){
      #If the second index is empty. eg: A[1,]
      if(!is.null(args$j)&&args$j=="")
        return(R_ncol(varInfo,refVar))
      #If there is no second index. eg:A[1]
      if(is.null(args$j))
        return(1)
      
      subs=args$j
      if(isNumeric(subs))
        return(1)
      
      return(R_length(varInfo,subs))
    }
  }
  
  
  #If the variable is just a variable and is not a lazy reference
  #curInfo is obtained above
  #curInfo=getVarInfo(varInfo,var,1)
  
  if(curInfo$isSeq){
    if(ind==2)
      return(1)
    if(ind==1){
      seqExp=parse(text=curInfo$specialContent)[[1]]
      from=seqExp[[2]]
      to=seqExp[[3]]
      by=seqExp[[4]]
      from_C=R_oneIndex_exp_sub(varInfo,from,k=1,k_C=TRUE)
      to_C=R_oneIndex_exp_sub(varInfo,to,k=1,k_C=TRUE)
      by_C=R_oneIndex_exp_sub(varInfo,by,k=1,k_C=TRUE)
      
      extCode=c(from_C$extCode,by_C$extCode,to_C$extCode)
      if(!is.null(extCode))
        stop("This type of code is not supported: ",deparse(seqExp))
      #Manually simplify the length
      part1=CSimplify(paste0(to_C$value,"/",by_C$value))
      part2=CSimplify(paste0(from_C$value,"/",by_C$value))
      if(!xor(isNumeric(part1),isNumeric(part2))){
        res=CSimplify(paste0("floor((",GPUVar$default_float,")(",part1,"-",part2,"))+1"))
      }else{
        if(isNumeric(part1)){
          res=CSimplify(paste0("-floor((",GPUVar$default_float,")(",part2,"))+",part1,"+1"))
        }else{
          if(isNumeric(part2)){
            res=CSimplify(paste0("floor((",GPUVar$default_float,")(",part1,"))-",part2,"+1"))
          }
        }
      }
      return(res)
    }
  }
  
  if(curInfo$dataType==T_scale) return(1)
  
  var_ind=varInfo$matrixInd[[var_char]]
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
