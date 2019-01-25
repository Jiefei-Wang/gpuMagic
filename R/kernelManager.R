#' Excute the openCL function
#' 
#' @examples 
#' #TODO
#' 
#' @export
.kernel<-function(src="",kernel,parms,
                  .device="auto",.globalThreadNum="length(FirstArg)",.options=kernel.getOption()){
  verbose=.options$verbose
  kernelMsg=.options$kernelMsg
  kernelOption=.options$kernelOption
    
  if(verbose||sum(as.matrix(kernelMsg[,-grep("warning",colnames(kernelMsg))]))!=0) 
    message("======kelnel compilation=========")
  #Read the opencl code
  codePack=readCode(src)
  src=codePack$src
  srcSig=codePack$srcSig
  
  #Find the device and platform id
  if(length(.device)>1) stop("Multiple devices are not supported!")
  if(.device=="auto"){
    deviceId=getFirstSelectedDevice()
  }else{
    deviceId=.device
  }
  device=getSelectedDevice(deviceId)
  
  
  dataType=c()
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list"){
      dataType[i]=parms[[i]]$type
      next
    }
    if(class(parms[[i]])!="gpuMatrix"){
      parms[[i]]=gpuMatrix(parms[[i]],device=deviceId)
    }
    dataType[i]=.type(parms[[i]])
    if(.device(parms[[i]])!=deviceId)
      stop("The data is not in the same device!")
  }
  
  
  
  #Create the signature for the kernel function
  sig=paste0(srcSig,kernelOption$flag,kernelOption$signature)
  
  
  #Create the data type macros
  #Add the signature if needed
  if(kernelOption$autoType&&length(dataType)!=0){
    gAUTO=paste0("#define gAuto",1:length(dataType)," global ",dataType,"\n",collapse = "")
    lAUTO=paste0("#define lAuto",1:length(dataType)," local ",dataType,"\n",collapse = "")
    pAUTO=paste0("#define auto",1:length(dataType)," ",dataType,"\n",collapse = "")
    
    AUTOVector=paste0("#define auto",1:length(dataType),"_v4 ",dataType,"4","\n",collapse = "")
    
    src=paste0(gAUTO,lAUTO,pAUTO,AUTOVector,src)
    sig=c(sig,paste0(dataType,collapse = ""))
  }
  
  sig_hash=digest(sig)
  #Create the kernel if it does not exist
  if(!hasKernel(device,sig_hash,kernel)){
    if(verbose||kernelMsg$compilation.msg)
      message("OpenCL compiler message: The kernel does not exist and will be created")
    .Call(C_createKernel,device[1],device[2],sig_hash,kernelOption$flag,src,kernel)
  }
  #Compute the usage of the shared memory and global memory
  #upload the parameters
  global_memory=0
  share_memory=0
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list"){
      share_memory=share_memory+parms[[i]]$size
      
      .Call(C_setSharedParameter,device[1],device[2],sig_hash,kernel,
         as.integer(parms[[i]]$size),as.integer(i-1))
    }else{
      global_memory=global_memory+getSize(parms[[i]])
      #message(getSize(parms[[i]]))
      .Call(C_setParameter,device[1],device[2],sig_hash,kernel,
         .getAddress(parms[[i]]),as.integer(i-1))
    }
  }
  if(verbose||kernelMsg$memory.usage.msg){
    message("OpenCL memory usage report:")
    message("Global memory: ",format_memory_size_output(global_memory))
    message("Shared memory: ",format_memory_size_output(share_memory))
  }
  
  if(.globalThreadNum=="length(FirstArg)"){
    .globalThreadNum=length(parms[[1]])
  }
  
  if(kernelOption$localThreadNum=="Auto"){
    localThreadNum=0
    localThreadNum_output=getGroupSize(device,sig_hash,kernel)
  }else{
    localThreadNum=kernelOption$localThreadNum
    localThreadNum_output=kernelOption$localThreadNum
  }
  
  
  if(verbose||kernelMsg$thread.num.msg){
    message("OpenCL thread Number report:")
    message(paste0("Total thread number: ",.globalThreadNum))
    message(paste0("block number: ",.globalThreadNum/localThreadNum_output))
    message(paste0("Thread number per block: ",localThreadNum_output))
  }
  
  .Call(C_launchKernel,device[1],device[2],sig_hash,kernel,as.integer(.globalThreadNum),as.integer(localThreadNum))
  
  invisible()
}

#' Get the openCL compilation options
#' 
#' @examples 
#' opt=kernel.getOption()
#' opt
#' @export
kernel.getOption<-function(){
  curOp=list()
  curOp$verbose=FALSE
  
  curOp$kernelMsg=data.frame(
    compilation.msg=FALSE,
    memory.usage.msg=FALSE,
    thread.num.msg=FALSE,
    insufficient.thread.num.warning=TRUE)
  
  curOp$kernelOption=data.frame(
    localThreadNum="Auto",
    signature="",
    flag="",
    autoType=TRUE,
    stringsAsFactors=FALSE)
  
  curOp
}

kernel.getSharedMem<-function(length,type){
  checkTypeSupport(type)
  return(list(length=length,size=length*getTypeSize(type),type=type))
}

readCode<-function(src){
  #if the src can be treated as a file name, then read the file, 
  #otherwise check if it is code.
  if(nchar(src)<128&&file.exists(src)){
    fileName=src
    ##Read source file
    src=readChar(fileName, file.info(fileName)$size)
    sig=as.character(file.mtime(fileName))
    #Add a space to make it more stable
    src=paste0(" ",src)
  }else{
    if(nchar(src)<40){
      if(length(grep("\\(.*\\)",src))==0){
        warning("It looks like the src variable is a file address, but I cannot find it.")
      }
    }
    sig=digest(src)
  }
  return(list(src=src,srcSig=sig))
}


#Check if the kernel is already exist
hasKernel<-function(device,sig,kernel){
  res=.Call(C_hasKernel,device[1],device[2],sig,kernel)
  res
}

getGroupSize<-function(device,sig,kernel){
  res=.Call(C_getPreferredGroupSize,device[1],device[2],sig,kernel)
  res
}
