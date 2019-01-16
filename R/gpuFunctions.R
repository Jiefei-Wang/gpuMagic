
checkTypeSupport<-function(type){
  if(!(type %in% gpuMagic$getAvailableType()))
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
    return(as.character(gpuMagic$getOptions("default.int",F)))
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

prettyPrint<-function(x){
  name=StrAlign(colnames(x),sep="\\l")
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
#' @param i numeric The device index
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
  prettyPrint(deviceInfo)
  invisible()
}

#' Get the current used device
#' @export
getCurDevice=function(){
  curInd=as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
  for(i in curInd){
    getDeviceInfo(i)
    cat("\n\n")
  }
  invisible()
}
#' Set which device will be used in the opencl, call 'getDeviceList()' first to figure out the index before use this function
#' @param i numeric The device index
#' @export
setDevice=function(i){
  selectDevice(sort(unique(as.integer(i))))
  invisible()
}

#' Query the current job status in a device
#' @param i numeric The device index
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
