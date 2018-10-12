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

#' @export
.kernel<-function(file,kernel,...,autoType=TRUE,blockNum="length(FirstArg)",threadNum=1){
  ##Read source file
  src=readChar(file, file.info(file)$size)
  src=cleanCode(src)
  #Add a space to make it more stable
  src=paste0(" ",src)
  ##Performing auto type conversion, tranfer R matrix and vector to GPUmatrix class
  parms=list(...)
  timeSig=as.character(file.mtime(file))
  res=parseProgram(timeSig,src,parms,autoType)
  src=res$src
  sig=res$sig
  parms=res$parms
  if(!hasKernel(sig,kernel)){
    message("kernel not exist")
    if(autoType)
      src=kernelSub(kernel,src,typeList)
    #Remove the other unidentified auto type
    src=gsub("([^a-zA-Z0-9_])(AUTO)([0-9]+[^a-zA-Z0-9_])","\\1double\\3",src)
    .C("createKernel",sig,kernel,src)
  }
  for(i in seq_len(length(parms))){
    .C("loadParameter",sig,kernel,.getAddress(parms[[i]]),as.integer(i-1))
  }
  if(blockNum=="length(FirstArg)"){
    blockNum=length(.data(parms[[1]]))
  }
  .C("launchKernel",sig,kernel,as.integer(blockNum),as.integer(threadNum))
  list(src=src)
}

parseProgram<-function(timeSig,src,parms,autoType=TRUE){
  typeList=c()
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])!="gpuMatrix")
      parms[[i]]=gpuMatrix(parms[[i]],T_F64)
    type=getTypeCXXStr(.type(parms[[i]]))
    typeList[i]=type
    parms[[i]]@gpuAddress$setReadyStatus(FALSE)
  }
  sig=paste0(c(timeSig,autoType,typeList),collapse = "")
  if(!hasKernel(sig,kernel)){
    if(autoType)
      src=kernelSub(kernel,src,typeList)
  }
  
  list(src=src,sig=sig,parms=parms)
}

kernelSub<-function(kernel,src,typeList){
  ind=regexpr("void vector_add[(].*?[)]",src)
  code=substr(src,ind,ind+attr(ind,"match.length"))
  src=gsub("void vector_add[(].*?[)]","PRESERVED_FUNCTION_CALL",src)
  if(length(typeList)>0)
  for(i in 1:length(typeList)){
    pattern=paste0("([^a-zA-Z0-9_])(",paste0("AUTO",i),")([^a-zA-Z0-9_])")
    x1=paste0("\\1 __global ",typeList[i],"\\3 ")
    x2=paste0("\\1",typeList[i],"\\3")
    code=gsub(pattern,x1,code)
    src=gsub(pattern,x1,src)
  }
  code=paste0("__kernel ",code)
  src=gsub("PRESERVED_FUNCTION_CALL",code,src)
}

