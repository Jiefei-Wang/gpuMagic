
checkTypeSupport<-function(type){
  if(!(type %in% gpuMagic.getAvailableType()))
    stop("The variable type ", type," is not supported")
}
getTypeNum<-function(type){
  switch(
    type,
    bool=1L,
    char=1L,
    half=2L,
    float=3L,
    double=4L,
    int=5L,
    long=6L,
    uint=7L,
    ulong=8L,
    stop("invalid type: ",type)
  )
}


getTypeCXXStr<-function(type){
  switch(
    type,
    bool="bool",
    char="char",
    half="half",
    float="float",
    double="double",
    int="int",
    long="long",
    uint="uint",
    ulong="ulong",
    stop("invalid type: ",type)
  )
}

#in byte
getTypeSize<-function(type){
  switch(
    type,
    bool=1L,
    char=1L,
    half=2L,
    float=4L,
    double=8L,
    int=4L,
    long=8L,
    uint=4L,
    ulong=8L,
    stop("invalid type: ",type)
  )
}
getDataType<-function(data){
  if(typeof(data)=="double"||typeof(data)=="numeric")
    return(GPUVar$default_float)
  if(typeof(data)=="integer")
    return(as.character(gpuMagic$getOptions("default.int")))
  stop("The given type is not defined")
}
convertDataType<-function(data,type){
  checkTypeSupport(type)
  switch(
    type,
    bool=as.raw(data),
    char=as.raw(data),
    int=as.integer(data),
    as.double(data)
  )
}



getPlatformNum<-function(){
  res=.Call(C_getPlatformNum)
  res
}
getDeviceNum<-function(platform){
  res=.Call(C_getDeviceNum,as.integer(platform))
  res
}
getSingleDeviceInfo<-function(platform,device){
  deviceName=paste0(rep(" ",100),collapse = "")
  deviceType=0L
  global_memory=0.0
  local_memory=0.0
  haslocalMemory=0L
  opencl_version=paste0(rep(" ",100),collapse = "")
  compute_unit_num=0L
  deviceInfo=.Call(C_getDeviceInfo,as.integer(platform),as.integer(device))
  names(deviceInfo)=c("deviceName","deviceType",
                      "globalMemory","localMemory","haslocalMemory",
                      "opencl_version","compute_unit_num")
  deviceInfo=as.data.frame(deviceInfo,stringsAsFactors=FALSE)
  deviceInfo=cbind(data.frame(id=NA,platform=platform,device=device),deviceInfo)
  
  deviceInfo$deviceType=switch(as.character(deviceInfo$deviceType),"0"="CPU","1"="GPU","2"="other")
  
  deviceInfo
}

print.options<-function(x){
  x=unlist(x)
  name=StrAlign(names(x),sep="\\l")
  value=StrAlign(as.character(x),sep="\\l")
  final=paste0(paste(name,value,sep=": "),collapse = "\n")
  cat(final)
}

#===========================Obtain device infomation==============
#' The function is used to obtain all the opencl-enable devices
#' @export
getDeviceList=function(){
  updateDeviceInfo()
  deviceInfo=.gpuResourcesManager$globalVars$deviceInfo[,c("id","platform","device","deviceName","globalMemory")]
  deviceInfo$globalMemory=sapply(deviceInfo$globalMemory,format_memory_size_output)
  print(deviceInfo, row.names = FALSE,right=F)
  invisible()
}

#' Get the ith device information, call 'getDeviceList()' first to figure out the index before use this function
#' @param i integer The device index
#' @export
getDeviceInfo=function(i){
  updateDeviceInfo()
  deviceInfo=.gpuResourcesManager$globalVars$deviceInfo
  if(i>nrow(deviceInfo)||i<=0){
    stop("Invalid device id!")
  }
  deviceInfo=deviceInfo[i,,drop=F]
  deviceInfo$globalMemory=format_memory_size_output(deviceInfo$globalMemory)
  deviceInfo$localMemory=format_memory_size_output(deviceInfo$localMemory)
  deviceInfo=structure(deviceInfo,class="options")
  deviceInfo
}

#' Get the information of the current devices
#' @export
getCurDevice=function(){
  curInd=as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
  for(i in curInd){
    print(getDeviceInfo(i))
    cat("\n\n")
  }
  invisible()
}
#' Set which device will be used in the opencl, call 'getDeviceList()' first to figure out the index before use this function
#' @param i integer The device index
#' @export
setDevice=function(i){
  selectDevice(sort(unique(as.integer(i))))
  invisible()
}
#' Get the index of the current devices 
#' @export
getDeviceIndex=function(){
  as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
}



#' Query the current job status in a device
#' @param i integer The device index
#' @export
getJobStatus=function(i){
  device=getSelectedDevice(i)
  status=.Call(C_getDeviceStatus,device[1],device[2])
  switch(as.character(status),
         "3"="queued",
         "2"="submitted",
         "1"="running",
         "0"="complete",
         paste0("Unknown status:",status))
}





#===========================Package functions==============

gpuMagic.options=new.env()
gpuMagic.options$default.thread.num=64
gpuMagic.options$supportedType<-c("bool","char","half","float","double","int","long","uint","ulong")



#' Get the openCL options
#' 
#' The functions get the computing precision when compile the GPU code and the number of workers in a computing group.
#' 
#' The fields `default.float`, `default.int` and `default.index.type` are used to control the computing precision. 
#' When transferring data from R to GPU, if the data in R has a numeric or double storage mode,
#'`default.float` will be used to convert data type.
#' Similarly, If the data has an Integer storage model. `default.int` will be used.
#' 
#' `default.index.type` controls the variable type for the for loop index, variable dimension etc.  
#' 
#' `default.thread.num` is used to control the number of workers in a group in openCL. It is not expected to be changed unless you know what you are doing.
#' 
#' @param opt The options that the function will return. It can be either "all" or a vector of the option names.
#' 
#' @seealso [gpuMagic.setOptions()] for changing the options.
#' @export
gpuMagic.getOptions=function(opt="all"){
  allOpt=data.frame(default.float=GPUVar$default_float,
                    default.int=GPUVar$default_int,
                    default.index.type=GPUVar$default_index_type,
                    default.thread.num=gpuMagic.options$default.thread.num,
                    stringsAsFactors=F
  )
  if(opt=="all")
    curOpt=allOpt
  else
    curOpt=allOpt[,opt,drop=F]
  curOpt=structure(curOpt, class = "options")
  curOpt
}



#'  Set the openCL options
#' 
#' The functions set the computing precision when compile the GPU code and the number of workers in a computing group.
#' 
#' @param ... 
#' There are two possible ways to set the options. You can either provide
#' 
#' 1. A named argument which name is the same as the name of the options.
#' 
#' 2. An R object obtaining from `gpuMagic.getOptions()`
#' 
#' to change the options.
#' 
#' @seealso [gpuMagic.getOptions()] for the name of the options.
#' @export
gpuMagic.setOptions=function(...){
  parms=list(...)
  if(length(parms)==1&&class(parms[[1]])=="options"){
    parms=parms[[1]]
  }
  optNames=names(parms)
  for(i in optNames){
    value=parms[[i]]
    switch(i,
           default.float={
             checkTypeSupport(value)
             GPUVar$default_float=value
           },
           default.int={
             checkTypeSupport(value)
             GPUVar$default_int=value
           },
           default.index.type={
             checkTypeSupport(value)
             GPUVar$default_index_type=value
           },
           default.thread.num={
             if(is.numeric(value))
               gpuMagic.options$default.thread.num=value
             else
               stop("The function argument should be a numeric value")
           },
           stop("Unknown options: ", names(parms[i]))
    )
  }
}
#' Get all the available openCL variable type
#' 
#' @return A vector of all the available data type.
#' @export
gpuMagic.getAvailableType=function(){
  gpuMagic.options$supportedType
}

#' Get the device memory usage
#' 
#' The function will print the memory usage on the console
#' @export
gpuMagic.getMemUsage=function(){
  .gpuResourcesManager$getGPUusage()
}