#' @export
gpuSapply<-function(X,FUN,...,verbose=F){
  GPUcode2=.gpuSapply(X,FUN,...)

  .kernel(kernel=GPUcode2$kernel,parms=GPUcode2$all_parms,autoType=FALSE,src=GPUcode2$gpu_code,verbose = verbose,signature = runif(1))
  res=GPUcode2$all_parms$gpu_return_variable
  res=sync(res)
  res=as.vector(res)
  if(length(res)==length(X)){
    return(res)
  }else{
    return(matrix(res,ncol=length(X)))
  }
}

.gpuSapply<-function(X,FUN,...){
  #Check and match the parameter names
  parms=list(...)
  parms=matchParms(X,parms,FUN)
  
  codeMetaInfo=list()
  codeMetaInfo$Exp=funcToExp(FUN)$code
  codeMetaInfo$parms=parms
  codeMetaInfo1=RParser1(codeMetaInfo)
  codeMetaInfo2=RParser2(codeMetaInfo1)
  codeMetaInfo3=RParser3(codeMetaInfo2)
  profileMeta1=RProfiler1(codeMetaInfo3)
  profileMeta2=RProfiler2(profileMeta1)
  profileMeta3=RRecompiler(profileMeta2)
  
  GPUExp1=RCcompilerLevel1(profileMeta3)
  
  names(parms)[1]=GPUVar$gpu_worker_data
  GPUcode=completeProfileTbl(GPUExp1,parms)
  GPUcode1=completeGPUcode(GPUcode)
  GPUcode2=fillGPUdata(GPUcode1,parms)
  GPUcode2
}


#as.vector(res)-A-B
#cat(GPUcode2$gpu_code)

fillGPUdata<-function(GPUcode,parms){
  temp_parms=list()
  profile=GPUcode$varInfo$profile
  returnInfo=GPUcode$varInfo$returnInfo
  profile_matrix=profile[profile$dataType==T_matrix,]
  
  
  for(ind in names(parms)){
    i=which(profile$var==ind)
    curType=as.numeric(profile[i,]$precisionType)
    parms[[ind]]=gpuMatrix(parms[[ind]],type=curType)
  }
  
  
  temp_parms$gpu_tmp_var=0
  temp_parms$gpu_tmp_length_arg=0
  temp_parms$gpu_tmp_matrix_offSize=rep(0,nrow(profile_matrix))
  temp_parms$gpu_matrix_size1=rep(0,nrow(profile_matrix))
  temp_parms$gpu_matrix_size2=rep(0,nrow(profile_matrix))
  temp_parms$gpu_return_variable=0
  temp_parms$gpu_return_size=as.numeric(returnInfo$size1)*as.numeric(returnInfo$size2)
  tmp_type_length=getTypeSize(GPUVar$default_tmp_type)
  if(nrow(profile_matrix)>0){
    for(i in 1:nrow(profile_matrix)){
      
      curType=getTypeCXXStr(as.numeric(profile_matrix[i,]$precisionType))
      curTypeLen=getTypeSize(curType)
      curSize1=as.numeric(profile_matrix[i,]$size1)
      curSize2=as.numeric(profile_matrix[i,]$size2)
      temp_parms$gpu_matrix_size1[i]=curSize1
      temp_parms$gpu_matrix_size2[i]=curSize2
      if(profile_matrix[i,]$require=="N"){
        temp_parms$gpu_tmp_matrix_offSize[i]=temp_parms$gpu_tmp_length_arg
        temp_parms$gpu_tmp_length_arg=temp_parms$gpu_tmp_length_arg+curTypeLen*curSize1*curSize2/tmp_type_length
      }
    }
  }
  temp_parms$gpu_tmp_var=rep(0,temp_parms$gpu_tmp_length_arg*length(parms$gpu_worker_data))
  temp_parms$gpu_return_variable=rep(0,temp_parms$gpu_return_size*length(parms$gpu_worker_data))
  
  
  tmp_var_type_list=c((GPUVar$default_tmp_type),(T_I64),
                      (T_I64),
                      (T_I64),(T_I64),
                      (T_DEFAULT_float),(T_I64))
  for(i in 1:length(temp_parms)){
    if(length(temp_parms[[i]])==0)temp_parms[[i]]=0
    temp_parms[[i]]=gpuMatrix(temp_parms[[i]],type=tmp_var_type_list[[i]])
  }
  all_parms=c(parms,temp_parms)
  GPUcode$all_parms=all_parms
  GPUcode
}


completeGPUcode<-function(GPUcode){
  profile=GPUcode$varInfo$profile
  ind=which(profile$require=="Y")
  code=paste0("__kernel void ",GPUVar$functionName,GPUVar$functionCount,"(")
  for(i in ind){
    curName=profile[i,]$var
    curType=getTypeCXXStr(as.numeric(profile[i,]$precisionType))
    code=paste0(code,"__global ",curType,"* ",curName,",")
  }
  tmp_var_list=c(GPUVar$gpu_tmp_var,GPUVar$gpu_tmp_length_arg,
                 GPUVar$gpu_tmp_matrix_offSize,
                 GPUVar$gpu_matrix_size1,GPUVar$gpu_matrix_size2,
                 GPUVar$gpu_return_variable,GPUVar$gpu_return_size)
  tmp_var_type_list=c(getTypeCXXStr(GPUVar$default_tmp_type),getTypeCXXStr(T_I64),
                      getTypeCXXStr(T_I64),
                      getTypeCXXStr(T_I64),getTypeCXXStr(T_I64),
                      getTypeCXXStr(T_DEFAULT_float),getTypeCXXStr(T_I64))
  for(i in 1:length(tmp_var_list)){
    code=paste0(code,"__global ",tmp_var_type_list[i],"* ",tmp_var_list[i])
    if(i!=length(tmp_var_list))
      code=paste0(code,",")
  }
  #code=paste0(code,)
  code=paste0(code,"){\n",paste0(GPUcode$gpu_code,collapse = "\n"),"}")
  GPUcode$gpu_code=code
  GPUcode$kernel=paste0(GPUVar$functionName,GPUVar$functionCount)
  GPUcode
}

completeProfileTbl<-function(GPUExp1,parms){
  profile=GPUExp1$varInfo$profile
  ind=which(profile$require=="Y")
  for(i in ind){
    varName=profile[i,]$var
    var=parms[[varName]]
    if(class(var)=="gpuMatrix"){
      curPrecision=getTypeNum(.type(var))
      curDim=dim(var)
    }else{
      curPrecision=T_DEFAULT_float
      curDim=dim(as.matrix(var))
    }
    profile[i,]$size1=curDim[1]
    profile[i,]$size2=curDim[2]
    if(sum(curDim)==2){
      profile[i,]$value=var[1]
    }
  }
  ind=which(profile$require!="Y")
  for(i in ind){
    profile[i,]$size1=eval(parse(text=profile[i,]$size1))
    profile[i,]$size2=eval(parse(text=profile[i,]$size2))
  }
  returnInfo=GPUExp1$varInfo$returnInfo
  returnInfo$size1=eval(parse(text=returnInfo$size1))
  returnInfo$size2=eval(parse(text=returnInfo$size2))
  
  GPUExp1$varInfo$returnInfo=returnInfo
  GPUExp1$varInfo$profile=profile
  GPUExp1
  
}


matchParms<-function(X,parms,FUN){
  
  argNames=names(funcToExp(FUN)$args)
  loopVar_ind=which(argNames=="X")
  if(length(loopVar_ind)==0)
    loopVar=argNames[1]
  else
    loopVar="X"
  parms=c(list(loopVar=X),parms)
  names(parms)[1]=loopVar
  unmatchedName=setdiff(argNames,names(parms))
  parName=names(parms)
  for(i in 1:length(parName)){
    if(parName[i]==""){
      if(length(unmatchedName)>0){
        parName[i]=unmatchedName[1]
        unmatchedName=unmatchedName[-1]
      }else
        stop("The function arguments does not match")
    }
  }
  if(length(unmatchedName)>0){
    stop("The function arguments does not match")
  }
  names(parms)=parName
  parms
}