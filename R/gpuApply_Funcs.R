
saveGPUcode<-function(GPUcode){
  GPUcode_hash=GPUcode
  GPUcode_hash$parms=NULL
  for(i in 1:length(GPUcode_hash$varInfo)){
    if(class(GPUcode_hash$varInfo[[i]])=="hash")
      GPUcode_hash$varInfo[[i]]=copy(GPUcode_hash$varInfo[[i]])
  }
  GPUcode_hash$parmsName=names(GPUcode$parms)
  GPUcode_hash
}
loadGPUcode<-function(key,parms){
  GPUcode=gpuApplyFuncList[[key]]
  GPUcode[["parms"]]=parms
  names(GPUcode$parms)=GPUcode$parmsName
  return(GPUcode)
}


createSapplySignature<-function(parms,FUN,.macroParms,.device,.options){
  sig=.device
  parmsName=names(parms)
  
  #skip the first parameter(the parameter that will be looped on)
  for(i in seq_len(length(parms)-1)+1){
    #Type of the parameters
    varSig=""
    if(sum(dim(parms[[i]]))==2)
      varSig=paste0(varSig,T_scale)
    else
      varSig=paste0(varSig,T_matrix)
    #Precision type of the parameter when it is a gpuMatrix class
    if(class(parms[[i]])=="gpuMatrix"){
      varSig=paste0(varSig,parms[[i]]$type)
    }
    #When it is a macro, add the dim and data
    if(parmsName[i] %in% .macroParms)
      varSig=paste(varSig,paste0(dim(parms[[i]]),collapse = ","),digest(parms[[i]][]),sep=",")
    sig=c(sig,varSig)
  }
  #Default variable type
  sig=c(sig,paste(GPUVar$default_float,GPUVar$default_int,GPUVar$default_index_type,sep=","))
  #gpuSapply options
  sig=c(sig,digest(FUN),digest(.macroParms),digest(.options$sapplyOptimization))
  sig
}



fillGPUdata<-function(GPUcode1,.options,.device){
  parms=GPUcode1$parms
  varInfo=GPUcode1$varInfo
  
  #Convert all the parameters into the gpuMatrix objects
  for(varName in names(parms)){
    if(class(parms[[varName]])=="gpuMatrix"){
      if(.device(parms[[varName]])==.device){
        next
      }else{
        warning("You supplied a gpu memory object but it is not belong to the device that the code will be run on.")
        parms[[varName]]=as.matrix(parms[[varName]])
       }
    }
    curInfo=getVarInfo(varInfo,varName,1)
    curType=curInfo$precisionType
    parms[[varName]]=gpuMatrix(parms[[varName]],type=curType,device=.device)
  }
  
  
  kernel_args=list()
  kernel_args$gp_size1=rep(0,length(varInfo$matrix_gp))
  kernel_args$gs_size1=rep(0,length(varInfo$matrix_gs))
  kernel_args$lp_size1=rep(0,length(varInfo$matrix_lp))
  kernel_args$ls_size1=rep(0,length(varInfo$matrix_ls))
  
  kernel_args$gp_size2=rep(0,length(varInfo$matrix_gp))
  kernel_args$gs_size2=rep(0,length(varInfo$matrix_gs))
  kernel_args$lp_size2=rep(0,length(varInfo$matrix_lp))
  kernel_args$ls_size2=rep(0,length(varInfo$matrix_ls))
  
  kernel_args$gp_offset=rep(0,length(varInfo$matrix_gp))
  kernel_args$gs_offset=rep(0,length(varInfo$matrix_gs))
  kernel_args$lp_offset=rep(0,length(varInfo$matrix_lp))
  kernel_args$ls_offset=rep(0,length(varInfo$matrix_ls))
  #gp_totalsize, gp_matrixNum, return size
  kernel_args$sizeInfo=rep(0,3)
  
  
  i=1
  offset=0
  for(varName in varInfo$matrix_gp){
    curInfo=getVarInfo(varInfo,varName,1)
    curType=curInfo$precisionType
    typeSize=getTypeSize(curType)
    kernel_args$gp_size1[i]=as.numeric(curInfo$size1)
    kernel_args$gp_size2[i]=as.numeric(curInfo$size2)
    curSize=kernel_args$gp_size1[i]*kernel_args$gp_size2[i]*typeSize
    kernel_args$gp_offset[i]=offset
    offset=offset+curSize
    i=i+1
  }
  #Total size per worker
  kernel_args$sizeInfo[1]=offset
  #Matrix number per worker
  kernel_args$sizeInfo[2]=i-1
  gp_size=offset
  
  i=1
  offset=0
  for(varName in varInfo$matrix_gs){
    curInfo=getVarInfo(varInfo,varName,1)
    curType=curInfo$precisionType
    typeSize=getTypeSize(curType)
    kernel_args$gs_size1[i]=as.numeric(curInfo$size1)
    kernel_args$gs_size2[i]=as.numeric(curInfo$size2)
    if(curInfo$require)
      curSize=0
    else
      curSize=kernel_args$gs_size1[i]*kernel_args$gs_size2[i]*typeSize
    kernel_args$gs_offset[i]=offset
    offset=offset+curSize
    i=i+1
  }
  gs_size=offset
  
  i=1
  offset=0
  for(varName in varInfo$matrix_ls){
    curInfo=getVarInfo(varInfo,varName,1)
    curType=curInfo$precisionType
    typeSize=getTypeSize(curType)
    kernel_args$ls_size1[i]=as.numeric(curInfo$size1)
    kernel_args$ls_size2[i]=as.numeric(curInfo$size2)
    curSize=kernel_args$ls_size1[i]*kernel_args$ls_size2[i]*typeSize
    kernel_args$ls_offset[i]=offset
    offset=offset+curSize
    i=i+1
  }
  ls_size=offset
  
  i=1
  offset=0
  for(varName in varInfo$matrix_lp){
    curInfo=getVarInfo(varInfo,varName,1)
    curType=curInfo$precisionType
    typeSize=getTypeSize(curType)
    kernel_args$lp_size1[i]=as.numeric(curInfo$size1)
    kernel_args$lp_size2[i]=as.numeric(curInfo$size2)
    curSize=kernel_args$lp_size1[i]*kernel_args$lp_size2[i]*typeSize
    kernel_args$lp_offset[i]=offset
    offset=offset+curSize
    i=i+1
  }
  lp_size=offset
  
  if(!is.null(varInfo$returnInfo))
    kernel_args$sizeInfo[3]=as.numeric(varInfo$returnInfo$size1)*as.numeric(varInfo$returnInfo$size2)
  
  #add a 0 value if there is no value in the arguments
  for(var in names(kernel_args)){
    if(length(kernel_args[[var]])==0)
      kernel_args[[var]]=0
  }
  
  
  #Allocate the gpu memory
  totalWorkerNum=length(parms[[1]])
  IntType=GPUVar$default_index_type
  
  device_argument=list()
  device_argument$gp_data=gpuEmptMatrix(row=floor(gp_size*totalWorkerNum/4)+1,col=1,type="int",device=.device)
  device_argument$gp_size1=gpuMatrix(rep(kernel_args$gp_size1,totalWorkerNum),type=IntType,device=.device)
  device_argument$gp_size2=gpuMatrix(rep(kernel_args$gp_size2,totalWorkerNum),type=IntType,device=.device)
  device_argument$gp_offset=gpuMatrix(kernel_args$gp_offset,type=IntType,device=.device)
  
  device_argument$gs_data=gpuEmptMatrix(row=floor(gs_size/4)+1,type="int",device=.device)
  device_argument$gs_size1=gpuMatrix(kernel_args$gs_size1,type=IntType,device=.device)
  device_argument$gs_size2=gpuMatrix(kernel_args$gs_size2,type=IntType,device=.device)
  device_argument$gs_offset=gpuMatrix(kernel_args$gs_offset,type=IntType,device=.device)
  
  device_argument$ls_data=kernel.getSharedMem(ls_size+1,type="char")
  device_argument$ls_size1=kernel.getSharedMem(length(kernel_args$ls_size1),type=IntType)
  device_argument$ls_size2=kernel.getSharedMem(length(kernel_args$ls_size2),type=IntType)
  device_argument$ls_offset=kernel.getSharedMem(length(kernel_args$ls_offset),type=IntType)
  
  returnSize=kernel_args$sizeInfo[3]*totalWorkerNum
  if(returnSize==0)
    returnSize=1
  device_argument$return_var=gpuEmptMatrix(returnSize,type=GPUVar$default_float,device=.device)
  device_argument$sizeInfo=gpuMatrix(kernel_args$sizeInfo,type=IntType,device=.device)
  
  
  
  
  device_argument=c(parms,device_argument)
  GPUcode1$device_argument=device_argument
  GPUcode1
}

#add the function definition
completeGPUcode<-function(GPUcode){
  varInfo=GPUcode$varInfo
  profile=varInfo$profile
  GPUVar$functionCount=GPUVar$functionCount+1
  kernelName=paste0(GPUVar$functionName,"_",GPUVar$functionCount)
  #Fefine function name 
  code=paste0("kernel void ",kernelName,"(")
  #The function arguments
  kernel_arg_code=c()
  for(curName in varInfo$requiredVar){
    curInfo=getVarInfo(varInfo,curName,1)
    curType=curInfo$precisionType
    kernel_arg_code=c(kernel_arg_code,paste0("global ",curType,"* ",curName))
  }
  code=paste0(code,paste0(kernel_arg_code,collapse = ","))
  
  #The working memory space
  arg_prefix_list=c(
    "global","global","global","global",
    "global","global","global","global",
    "local","local","local","local",
    "global","global"
  )
  arg_list=c(GPUVar$global_private_data,paste0(GPUVar$global_private_size1,"_arg"),paste0(GPUVar$global_private_size2,"_arg"),GPUVar$global_private_offset,
             GPUVar$global_shared_data,GPUVar$global_shared_size1,GPUVar$global_shared_size2,GPUVar$global_shared_offset,
             GPUVar$local_shared_data,GPUVar$local_shared_size1,GPUVar$local_shared_size2,GPUVar$local_shared_offset,
             GPUVar$return_variable,GPUVar$size_info)
  arg_type_list=c("char",GPUVar$default_index_type,GPUVar$default_index_type,GPUVar$default_index_type,
                  "char",GPUVar$default_index_type,GPUVar$default_index_type,GPUVar$default_index_type,
                  "char",GPUVar$default_index_type,GPUVar$default_index_type,GPUVar$default_index_type,
                  GPUVar$default_float,GPUVar$default_index_type)
  for(i in 1:length(arg_list)){
    curCode=paste0(arg_prefix_list[i]," ",arg_type_list[i],"* ",arg_list[i])
    if(i!=length(arg_list))
      curCode=paste0(curCode)
    code=c(code,curCode)
  }
  
  
  
  code=paste0(code,collapse = ",\n")
  
  
  
  #add the kernel function definition
  code=paste0(code,"){\n",paste0(GPUcode$gpu_code,collapse = "\n"),"}")
  
  #Add the double vector support if appliable
  if(GPUVar$default_float=="double")
    code=paste0("#pragma OPENCL EXTENSION cl_khr_fp64：enable\n",code)
  
  GPUcode$gpu_code=code
  GPUcode$kernel=kernelName
  
  GPUcode
}

completeProfileTbl<-function(GPUExp2){
  varInfo=GPUExp2$varInfo
  profile=varInfo$profile
  parms=GPUExp2$parms
  for(varName in varInfo$requiredVar){
    curInfo=getVarInfo(varInfo,varName,1)
    var=parms[[varName]]
    
    if(class(var)=="gpuMatrix"){
      curPrecision=getTypeNum(.type(var))
      curDim=dim(var)
    }else{
      curPrecision=GPUVar$default_float
      curDim=dim(as.matrix(var))
    }
    curInfo$size1=curDim[1]
    curInfo$size2=curDim[2]
    varInfo=setVarInfo(varInfo,curInfo)
  }
  allVars=keys(varInfo$varVersion)
  nonRequiredVars=allVars[!allVars%in% varInfo$requiredVar]
  
  for(varName in nonRequiredVars){
    curInfo=getVarInfo(varInfo,varName,1)
    curInfo$size1=eval(parse(text=curInfo$size1))
    curInfo$size2=eval(parse(text=curInfo$size2))
    varInfo=setVarInfo(varInfo,curInfo)
  }
  returnVar=varInfo$returnInfo$var
  if(!is.null(returnVar)){
    returnInfo=getVarInfo(varInfo,returnVar)
    
    returnInfo$size1=eval(parse(text=returnInfo$size1))
    returnInfo$size2=eval(parse(text=returnInfo$size2))
    varInfo$returnInfo=returnInfo
  }
  
  GPUExp2$varInfo=varInfo
  GPUExp2
  
}
CheckCodeError<-function(GPUcode,parms){
  errorCheckInfo=GPUcode$varInfo$errorCheck
  for(i in keys(errorCheckInfo)){
    if(!isNumeric(i)) next
    info=errorCheckInfo[[i]]
    error=eval(parse(text=info$check))
    if(error){
      if(info$level=="warning"){
        warning(info$msg,": \n",info$code)
      }else{
        stop(info$msg,": \n",info$code)
      }
    }
  }
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

formatParms<-function(parms){
  for(i in 1:length(parms)){
    if(class(parms[[i]])!="gpuMatrix"&&class(parms[[i]])!="matrix"){
      parms[[i]]=as.matrix(parms[[i]])
    }
  }
  parms
}

#=========================optimization functions==============================
opt_workerNumber<-function(varInfo,code,.options){
  targetCode=paste0("//Thread number optimization\n")
  
  if(!grepl(targetCode,code,fixed = T)){
    stop("Unable to find the location of the thread number optimization code\n",
         "This error should never be happened\n",
         "Please contact the author")
  }
  
  if(.options$sapplyOptimization$thread.number){
    insertedCode=paste0("if(",GPUVar$gpu_global_id,"<",R_length(varInfo,GPUVar$gpu_loop_data),"){\n")
    insertedCode=paste0(targetCode,insertedCode)
    endCode="\n}"
  }else{
    insertedCode=""
    endCode=""
  }
  code=sub(targetCode,insertedCode,code,fixed = T)
  code=paste0(code,endCode)
  code
}

opt_matrixDim<-function(varInfo,code,.options){
  targetCode=paste0("//Matrix dimension optimization\n")
  
  if(!grepl(targetCode,code,fixed = T)){
    stop("Unable to find the location of the thread number optimization code\n",
         "This error should never be happened\n",
         "Please contact the author")
  }
  if(!.options$sapplyOptimization$matrix.dim){
    code=sub(targetCode,"",code,fixed = T)
    return(code)
  }
  
  match.info=regexpr(targetCode,code,fixed = T)
  opt_code=substring(code,match.info+attr(match.info,"match.length"))
  
  opt_target_space=c("gp","gs","lp","ls")
  opt_target=c(
    paste0("gpu_",opt_target_space,"_size1"),
    paste0("gpu_",opt_target_space,"_size2")
  )
  variable_definition=c()
  for(i in 1:length(opt_target)){
    res=opt_matrixDim_hidden(opt_code,opt_target[i])
    variable_definition=c(variable_definition,res$variable_definition)
    opt_code=res$code_optimization
  }
  variable_definition=paste0(variable_definition,collapse = "")
  code=paste0(
    substr(code,1,match.info),
    targetCode,
    variable_definition,
    opt_code
  )
  
  code
}
#opt.target=opt_target[i]

opt_matrixDim_hidden<-function(code,opt.target){
  target.reg=paste0(opt.target,"\\[([0-9]+)\\]")
  target.replace=paste0(opt.target,"_\\1")
  
  variable=str_match_all(code,target.reg)[[1]][,1]
  if(length(variable)==0)
    return(list(code_optimization=code))
  
  variable=unique(variable)
  variable_opt=gsub(target.reg,target.replace,variable)
  
  variable_definition=paste0(GPUVar$default_index_type," ",variable_opt,"=",variable,";\n")
  variable_definition=paste0(variable_definition,collapse = "")
  code_optimization=gsub(target.reg,target.replace,code)
  
  return(list(variable_definition=variable_definition,code_optimization=code_optimization))
}

