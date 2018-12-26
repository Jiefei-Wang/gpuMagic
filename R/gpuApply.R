gpuApplyFuncList=hash()

#' @export
gpuSapply<-function(X,FUN,...,.macroParms=NULL,.options=gpuSapply.getOption()){
  #Some interesting setup
  start_time <- Sys.time()
  verbose=.options$verbose
  optimization=.options$sapplyOptimization
  msg=.options$sapplyMsg
  option=.options$sapplyOption
  
  
  if(verbose||sum(as.matrix(msg))!=0) 
    message("======gpuSapply compilation======")
  #Check and match the parameter names
  parms=list(...)
  parms=matchParms(X,parms,FUN)
  #Convert all the parameters to matrix type
  parms=formatParms(parms)
  #Create the signature for the functions
  sig=createSapplySignature(parms,FUN,.macroParms,.options)
  sig_hash=digest(sig)
  #Check if the compiled code exist, if not, compile the function
  if(has.key(sig_hash,gpuApplyFuncList)&&option$debugCode==""){
    if(verbose||msg$R.code.compiler.msg){
      message("The R function has been compiled.")
    }
    GPUcode1=gpuApplyFuncList[[sig_hash]]
    #GPUcode1=as.list(gpuApplyFuncList)[[1]]
    GPUcode1[["parms"]]=parms
    names(GPUcode1$parms)=GPUcode1$parmsName
  }else{
    if(verbose||msg$R.code.compiler.msg){
      message("The R function has not been compiled.")
    }
    GPUcode1=.compileGPUCode(FUN,parms,.macroParms=.macroParms,.options=.options)
    if(option$debugCode!=""){
      GPUcode1$gpu_code=option$debugCode
      GPUcode1$kernel=gsub("kernel void ([^(]+)\\(.+","\\1",option$debugCode)
    }
    #Store the GPU object
    gpuApplyFuncList[sig_hash]=saveGPUcode(GPUcode1)
  }
  
  
  
  #Complete the profile table and fill the GPU data
  GPUcode1=completeProfileTbl(GPUcode1)
  CheckCodeError(GPUcode1,parms)
  GPUcode2=fillGPUdata(GPUcode1,.options=.options)
  
  
  if(optimization$thread.number==TRUE){
    if(.options$kernelOption$localThreadNum=="Auto")
      kernelNum=gpuMagic.option$getDefaultThreadNum()
    else
      kernelNum=.options$kernelOption$localThreadNum
    
    .globalThreadNum=ceiling(length(X)/kernelNum)*kernelNum
    .options$kernelOption$localThreadNum=kernelNum
  }else{
    .globalThreadNum=length(X)
  }
  
  if(verbose||msg$timing.R.code.compilation){
    end_time <- Sys.time()
    compileTime=round(as.numeric(end_time-start_time),digits=3)
    message("Total R code compilation time: ",compileTime," secs")
  }
  
  #.options$signature=c(.options$signature,sig_hash)
  .kernel(kernel=GPUcode2$kernel,src=GPUcode2$gpu_code,parms=GPUcode2$device_argument,
          .globalThreadNum=.globalThreadNum,.options=.options)
  res=GPUcode2$device_argument$return_var
  res=download(res)
  res=as.vector(res)
  if(length(res)==length(X)){
    return(res)
  }else{
    return(matrix(res,ncol=length(X)))
  }
}

gpuSapply.getOption<-function(){
  curOp=kernel.getOption()
  curOp$kernelOption$autoType=FALSE
  curOp$sapplyMsg=
    data.frame(
      R.code.compiler.msg=F,
      timing.R.code.compilation=F
      )
  
  curOp$sapplyOptimization=
    data.frame(
      thread.number=T,
      matrix.dim=T
      )
  
  curOp$sapplyOption=data.frame(debugCode="")
  
  
  return(curOp)
}

#This function is for user to call
#' @export
compileGPUCode<-function(X,FUN,...,.macroParms=NULL,.options=gpuSapply.getOption()){
  parms=list(...)
  parms=matchParms(X,parms,FUN)
  parms=formatParms(parms)
  GPUcode1=.compileGPUCode(FUN,parms,.macroParms=.macroParms,.options=.options)
}

.compileGPUCode<-function(FUN,parms,.macroParms=NULL,.options){
  codeMetaInfo=list()
  codeMetaInfo$Exp=funcToExp(FUN)$code
  codeMetaInfo$parms=parms
  codeMetaInfo$macroParms=.macroParms
  
  
  codeMetaInfo0=codePreprocessing(codeMetaInfo)
  codeMetaInfo1=RParser1(codeMetaInfo0)
  codeMetaInfo2=RParser2(codeMetaInfo1)
  profileMeta1=RProfile1(codeMetaInfo2)
  profileMeta2=RProfile2(profileMeta1)
  #profileMeta3=RRecompiler(profileMeta2)
  GPUExp1=RCcompilerLevel1(profileMeta2)
  GPUExp2=RCcompilerLevel2(GPUExp1)
  
  
  GPUcode1=completeGPUcode(GPUExp2)
  
  #optimization
  GPUcode1$gpu_code=opt_workerNumber(GPUcode1$varInfo,GPUcode1$gpu_code,.options)
  GPUcode1$gpu_code=opt_matrixDim(GPUcode1$varInfo,GPUcode1$gpu_code,.options)
  
  
  GPUcode1
}

