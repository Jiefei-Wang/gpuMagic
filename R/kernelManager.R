
#' @export
.kernel<-function(file="",kernel,parms,
                  .globalThreadNum="length(FirstArg)",.options=kernel.getOption(),
                  src=""){
  verbose=.options$verbose
  kernelMsg=.options$kernelMsg
  kernelOption=.options$kernelOption
    
  if(verbose||sum(as.matrix(kernelMsg[,-grep("warning",colnames(kernelMsg))]))!=0) 
    message("======kelnel compilation=========")
  #Read the opencl code
  codePack=readCode(file,src)
  src=codePack$src
  
  
  device=getCurDeviceIndex()
  dataType=c()
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list"){
      dataType[i]=parms[[i]]$type
      next
    }
    if(class(parms[[i]])!="gpuMatrix"){
      parms[[i]]=gpuMatrix(parms[[i]])
    }
    dataType[i]=.type(parms[[i]])
    if(parms[[i]]@gpuAddress$getDevice()!=device)
      stop("The data is not in the same device!")
  }
  
  
  
  #Create the signature for the kernel function
  sig=paste0(codePack$timeSig,kernelOption$flag,kernelOption$signature)
  
  
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
  
  if(!hasKernel(sig_hash,kernel)){
    if(verbose||kernelMsg$compilation.msg)
      message("OpenCL compiler message: The kernel does not exist and will be created")
    .C("createKernel",sig_hash,kernel,src,kernelOption$flag)
  }
  #Compute the usage of the shared memory and global memory
  #upload the parameters
  global_memory=0
  share_memory=0
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list"){
      share_memory=share_memory+parms[[i]]$size
      
      .C("loadSharedParameter",sig_hash,kernel,
         as.integer(parms[[i]]$size),as.integer(i-1))
    }else{
      global_memory=global_memory+getSize(parms[[i]])
      #message(getSize(parms[[i]]))
      .C("loadParameter",sig_hash,kernel,.getAddress(parms[[i]]),
         as.integer(i-1))
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
  minBlock=16
  if(kernelOption$localThreadNum=="Auto"){
    Block=.globalThreadNum
    localThreadNum=1
    repeat{
      if(Block%%2==0&&localThreadNum<64&&Block>=minBlock*2){
        localThreadNum=localThreadNum*2
        Block=Block/2
      }else{
        break
      }
    }
  }else{
    localThreadNum=kernelOption$localThreadNum
  }
  if(localThreadNum<=32&&Block>=minBlock&&
     (verbose||kernelMsg$insufficient.thread.num.warning)){
    warning(paste0("The current thread number is ",localThreadNum,". This may have negative effect on the performance. Please consider to increase the thread number"))
  }
  
  if(verbose||kernelMsg$thread.num.msg){
    message("OpenCL thread Number report:")
    message(paste0("Total thread number: ",.globalThreadNum))
    message(paste0("Block number: ",.globalThreadNum/localThreadNum))
    message(paste0("Thread number per block: ",localThreadNum))
  }
  
  .C("launchKernel",sig_hash,kernel,as.integer(.globalThreadNum),as.integer(localThreadNum))
  invisible()
}
#' @export

kernel.getOption<-function(){
  curOp=list()
  curOp$verbose=FALSE
  
  curOp$kernelMsg=data.frame(
    compilation.msg=F,
    memory.usage.msg=F,
    thread.num.msg=F,
    insufficient.thread.num.warning=T)
  
  curOp$kernelOption=data.frame(
    localThreadNum="Auto",
    signature="",
    flag="",
    autoType=TRUE,
    stringsAsFactors=F)
  
  curOp
}

kernel.getSharedMem<-function(length,type){
  checkTypeSupport(type)
  return(list(length=length,size=length*getTypeSize(type),type=type))
}

readCode<-function(file,src){
  if(file!=""){
    ##Read source file
    src=readChar(file, file.info(file)$size)
    #src=cleanCode(src)
    #Add a space to make it more stable
    src=paste0(" ",src)
    timeSig=as.character(file.mtime(file))
  }else{
    if(src!=""){
      #src=cleanCode(src)
      #Add a space to make it more stable
      src=paste0(" ",src)
      timeSig=src
    }else{
      stop("Does not find the kernel code")
    }  
  }
  list(timeSig=timeSig,src=src)
}

parseProgram<-function(codePack,kernel,parms,autoType=TRUE){
  typeList=c()
  
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])!="gpuMatrix")
      type=getTypeCXXStr(getDataType(parms[[i]]))
    else
      type=getTypeCXXStr(.type(parms[[i]]))
    typeList[i]=type
  }
  sig=paste0(c(codePack$timeSig,autoType,typeList),collapse = "")
  if(!hasKernel(sig,kernel)){
    if(autoType)
      codePack$src=kernelSub(kernel,codePack$src,typeList)
  }
  
  list(src=codePack$src,sig=sig)
}


#Clean The code to make the kernel code more stable
#Clean comments, tabs and unnecessary wrap.
cleanCode<-function(src){
  src=gsub("//.*?\n","",src)
  src=gsub("\t","",src)
  src=gsub("\r","",src)
  src=gsub("\n+"," ",src)
  
  src
}
#Check if the kernel is already exist
hasKernel<-function(sig,kernel){
  res=.C("hasKernel",sig,kernel,FALSE)
  res[[3]]
}

