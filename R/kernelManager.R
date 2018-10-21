
#' @export
.kernel<-function(file="",kernel,parms,autoType=TRUE,globalThreadNum="length(FirstArg)",localThreadNum="Auto",src="",verbose=FALSE,signature=""){
  #message(globalThreadNum)
  codePack=readCode(file,src)
  #message(codePack)
  ##Performing auto type conversion, tranfer R matrix and vector to GPUmatrix class
  
  res=parseProgram(codePack,kernel,parms,autoType)
  src=res$src
  sig=paste0(res$sig,signature,collapse = "")
  
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])!="gpuMatrix")
      parms[[i]]=gpuMatrix(parms[[i]])
    parms[[i]]@gpuAddress$setReadyStatus(FALSE)
  }
  
  if(!hasKernel(sig,kernel)){
    if(verbose)
      message("The given kernel does not exist and will be created")
    src=gsub("([^a-zA-Z0-9_])(AUTO)([0-9]+[^a-zA-Z0-9_])","\\1double\\3",src)
    #message(src)
    .C("createKernel",sig,kernel,src)
  }
  for(i in seq_len(length(parms))){
    .C("loadParameter",sig,kernel,.getAddress(parms[[i]]),as.integer(i-1))
  }
  if(globalThreadNum=="length(FirstArg)"){
    globalThreadNum=length(.data(parms[[1]]))
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
    message(paste0("Block number: ",Block))
    message(paste0("Thread number per block: ",localThreadNum))
  }
  
  .C("launchKernel",sig,kernel,as.integer(globalThreadNum),as.integer(localThreadNum))
  invisible()
}

readCode<-function(file,src){
  if(file!=""){
    ##Read source file
    src=readChar(file, file.info(file)$size)
    src=cleanCode(src)
    #Add a space to make it more stable
    src=paste0(" ",src)
    timeSig=as.character(file.mtime(file))
  }else{
    if(src!=""){
      src=cleanCode(src)
      #Add a space to make it more stable
      src=paste0(" ",src)
      timeSig=""
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

