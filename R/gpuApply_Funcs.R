
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
  sig=c()
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


getVarSizeInfo_C_level<-function(sizeMatrix){
  matrixOffset=c()
  size1=sizeMatrix$size1
  size2=sizeMatrix$size2
  curoffSet=0
  for(i in seq_len(nrow(sizeMatrix))){
    curInfo=sizeMatrix[i,]
    matrixOffset[i]=curoffSet
    curoffSet=curoffSet+curInfo$totalSize
  }
  res=list(matrixOffset=matrixOffset,size1=size1,size2=size2,
           totalSize=curoffSet,matrixNum=nrow(sizeMatrix))
  return(res)
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
  
  
  sizeInfo_gp=getVarSizeInfo_C_level(varInfo$matrix_gp)
  kernel_args$gp_offset=sizeInfo_gp$matrixOffset
  kernel_args$gp_size1=sizeInfo_gp$size1
  kernel_args$gp_size2=sizeInfo_gp$size2
  gp_size=sizeInfo_gp$totalSize
  #Total size per worker
  kernel_args$sizeInfo[1]=sizeInfo_gp$totalSize
  #Matrix number per worker
  kernel_args$sizeInfo[2]=sizeInfo_gp$matrixNum
  
  
  sizeInfo_gs=getVarSizeInfo_C_level(varInfo$matrix_gs)
  kernel_args$gs_offset=sizeInfo_gs$matrixOffset
  kernel_args$gs_size1=sizeInfo_gs$size1
  kernel_args$gs_size2=sizeInfo_gs$size2
  gs_size=sizeInfo_gs$totalSize
  
  sizeInfo_ls=getVarSizeInfo_C_level(varInfo$matrix_ls)
  kernel_args$ls_offset=sizeInfo_ls$matrixOffset
  kernel_args$ls_size1=sizeInfo_ls$size1
  kernel_args$ls_size2=sizeInfo_ls$size2
  ls_size=sizeInfo_ls$totalSize
  
  sizeInfo_lp=getVarSizeInfo_C_level(varInfo$matrix_lp)
  kernel_args$lp_offset=sizeInfo_lp$matrixOffset
  kernel_args$lp_size1=sizeInfo_lp$size1
  kernel_args$lp_size2=sizeInfo_lp$size2
  lp_size=sizeInfo_lp$totalSize
  
  
  if(!is.null(varInfo$returnInfo)){
    returnInfo=varInfo$returnInfo
    returnSizeVector=returnInfo$size1*returnInfo$size2
    if(length(returnSizeVector)!=0){
      if(sum(returnSizeVector[1]!=returnSizeVector)>0)
        warning("Multiple return size has been found!")
      
      kernel_args$sizeInfo[3]=max(returnSizeVector)
    }
  }
  
  #add a 0 value if there is no value in the arguments
  for(var in names(kernel_args)){
    if(length(kernel_args[[var]])==0)
      kernel_args[[var]]=0
  }
  
  
  #Allocate the gpu memory
  totalWorkerNum=length(parms[[1]])
  IntType=GPUVar$default_index_type
  
  device_argument=list()
  device_argument$gp_data=gpuEmptMatrix(row=ceiling(gp_size*totalWorkerNum/4),col=1,type="int",device=.device)
  device_argument$gp_size1=gpuMatrix(rep(kernel_args$gp_size1,totalWorkerNum),type=IntType,device=.device)
  device_argument$gp_size2=gpuMatrix(rep(kernel_args$gp_size2,totalWorkerNum),type=IntType,device=.device)
  device_argument$gp_offset=gpuMatrix(kernel_args$gp_offset,type=IntType,device=.device)
  
  device_argument$gs_data=gpuEmptMatrix(row=ceiling(gs_size/4),type="int",device=.device)
  device_argument$gs_size1=gpuMatrix(kernel_args$gs_size1,type=IntType,device=.device)
  device_argument$gs_size2=gpuMatrix(kernel_args$gs_size2,type=IntType,device=.device)
  device_argument$gs_offset=gpuMatrix(kernel_args$gs_offset,type=IntType,device=.device)
  
  device_argument$ls_data=kernel.getSharedMem(ls_size+1,type="char")
  device_argument$ls_size1=kernel.getSharedMem(length(kernel_args$ls_size1),type=IntType)
  device_argument$ls_size2=kernel.getSharedMem(length(kernel_args$ls_size2),type=IntType)
  device_argument$ls_offset=kernel.getSharedMem(length(kernel_args$ls_offset),type=IntType)
  
  #The return size for each thread
  returnSize=kernel_args$sizeInfo[3]
  device_argument$return_var=gpuEmptMatrix(kernel_args$sizeInfo[3],totalWorkerNum,type=GPUVar$default_float,device=.device)
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

evaluateProfileTbl<-function(parms,table){
  if(is.null(table)||nrow(table)==0) return(table)
  table$totalSize=sapply(as.list(parse(text=table$totalSize)), eval,envir=environment())
  table$size1=sapply(as.list(parse(text=table$size1)), eval,envir=environment())
  table$size2=sapply(as.list(parse(text=table$size2)), eval,envir=environment())
  return(table)
}


completeProfileTbl<-function(GPUExp2){
  parms=GPUExp2$parms
  varInfo=GPUExp2$varInfo
  varInfo$matrix_gs=evaluateProfileTbl(parms,varInfo$matrix_gs)
  varInfo$matrix_gp=evaluateProfileTbl(parms,varInfo$matrix_gp)
  varInfo$matrix_ls=evaluateProfileTbl(parms,varInfo$matrix_ls)
  varInfo$matrix_lp=evaluateProfileTbl(parms,varInfo$matrix_lp)
  varInfo$returnInfo=evaluateProfileTbl(parms,varInfo$returnInfo)
  
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

