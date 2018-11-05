
#' @export
.kernel<-function(file="",kernel,parms,
                  globalThreadNum="length(FirstArg)",.options=kernel.getOption(),
                  src=""){
  autoType=.options$autoType
  localThreadNum=.options$localThreadNum
  signature=.options$signature
  verbose=.options$verbose
  flag=.options$flag
  #message(globalThreadNum)
  codePack=readCode(file,src)
  src=codePack$src
  #cat(src)
  #message(codePack)
  ##Performing auto type conversion, tranfer R matrix and vector to GPUmatrix class
  
  #res=parseProgram(codePack,kernel,parms,autoType)
  #src=res$src
  #sig=paste0(res$sig,signature,collapse = "")
  sig=paste0(flag,codePack$timeSig,signature)
  
  device=getCurDeviceIndex()
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list")
      next
    if(class(parms[[i]])!="gpuMatrix")
      parms[[i]]=gpuMatrix(parms[[i]])
    parms[[i]]@gpuAddress$setReadyStatus(FALSE)
    if(parms[[i]]@gpuAddress$getDevice()!=device)
      stop("The data is not in the same device!")
  }
  
  if(!hasKernel(sig,kernel)){
    if(verbose)
      message("The given kernel does not exist and will be created")
    #src=gsub("([^a-zA-Z0-9_])(AUTO)([0-9]+[^a-zA-Z0-9_])","\\1double\\3",src)
    #message(src)
    .C("createKernel",sig,kernel,src,flag)
  }
  share_memory=0
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])=="list"){
      .C("loadSharedParameter",sig,kernel,
         as.integer(parms[[i]]$size),as.integer(i-1))
      share_memory=share_memory+parms[[i]]$size
    }else{
      .C("loadParameter",sig,kernel,.getAddress(parms[[i]]),
         as.integer(i-1))
    }
  }
  if(verbose)
    message("Memory usage:\nShared memory: ",share_memory)
  if(globalThreadNum=="length(FirstArg)"){
    globalThreadNum=length(parms[[1]])
  }
  minBlock=16
  if(localThreadNum=="Auto"){
    Block=globalThreadNum
    localThreadNum=1
    repeat{
      if(Block%%2==0&&localThreadNum<64&&Block>=minBlock*2){
        localThreadNum=localThreadNum*2
        Block=Block/2
      }else{
        break
      }
    }
  }
  if(localThreadNum<=32&&Block>=minBlock&&verbose){
    warning(paste0("The current thread number is ",localThreadNum,". This may have negative effect on the performance. Please consider to increase the thread number"))
  }
  if(verbose){
    message(paste0("Total thread number: ",globalThreadNum))
    message(paste0("Block number: ",globalThreadNum/localThreadNum))
    message(paste0("Thread number per block: ",localThreadNum))
  }
  
  .C("launchKernel",sig,kernel,as.integer(globalThreadNum),as.integer(localThreadNum))
  invisible()
}
#' @export

kernel.getOption<-function(){
  list(autoType=TRUE,localThreadNum="Auto",
       signature="",verbose=FALSE,flag="")
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

kernelSub<-function(kernel,src,typeList){
  headerPattern=paste0("void ",kernel,"[(].*?[)]")
  ind=regexpr(headerPattern,src)
  code=substr(src,ind,ind+attr(ind,"match.length"))
  src=gsub(headerPattern,"PRESERVED_FUNCTION_CALL",src)
  if(length(typeList)>0)
  for(i in 1:length(typeList)){
    pattern=paste0("([^a-zA-Z0-9_])(",paste0("AUTO",i),")([^a-zA-Z0-9_])")
    x1=paste0("\\1 __global ",typeList[i],"\\3 ")
    #x2=paste0("\\1",typeList[i],"\\3")
    code=gsub(pattern,x1,code)
    src=gsub(pattern,x1,src)
  }
  code=paste0("__kernel ",code)
  src=gsub("PRESERVED_FUNCTION_CALL",code,src)
  src
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

