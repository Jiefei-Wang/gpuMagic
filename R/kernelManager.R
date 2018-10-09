cleanCode<-function(src){
  src=gsub("//.*?\n","",src)
  src=gsub("\t","",src)
  src=gsub("\n+"," ",src)
  
  src
}

.kernel<-function(file,kernel,...,autoType=TRUE,blockNum="length(FirstArg)",threadNum=1){
  ##Read source file
  src=readChar(file, file.info(file)$size)
  src=cleanCode(src)
  ##Performing auto type conversion, tranfer R matrix and vector to GPUmatrix class
  typeList=c()
  parms=list(...)
  if(autoType){
    for(i in seq_len(length(parms))){
      if(class(parms[[i]])!="gpuMatrix")
        parms[[i]]=gpuMatrix(parms[[i]],T_F64)
      
      pattern=paste0("([^a-zA-Z0-9_])(",paste0("AUTO",i),")([^a-zA-Z0-9_])")
      type=getTypeCXXStr(.type(parms[[i]]))
      typeList[i]=type
      x=paste0("\\1",type,"\\3")
      src=gsub(pattern,x,src)
    }
    src=gsub("([^a-zA-Z0-9_])(AUTO)([0-9]+[^a-zA-Z0-9_])","\\1double\\3",src)
  }else{
    for(i in seq_len(length(parms))){
      if(class(parms[[i]])!="gpuMatrix")
        parms[[i]]=gpuMatrix(parms[[i]],T_F64)
    }
  }
  sig=paste0(c(as.character(file.mtime(file)),autoType,typeList),collapse = "")
  .C("createKernel",sig,kernel,src)
  
  for(i in seq_len(length(parms))){
    .C("loadParameter",sig,kernel,.getAddress(parms[[i]]),as.integer(i-1))
  }
  if(blockNum=="length(FirstArg)"){
    blockNum=length(.data(parms[[1]]))
  }
  .C("launchKernel",sig,kernel,as.integer(blockNum),as.integer(threadNum))
  invisible()
}