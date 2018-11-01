
checkTypeSupport<-function(type){
  if(!(type %in% gpuMagic.option$getAvailableType()))
    stop("The variable type ", type," is not supported")
}
getTypeNum<-function(type){
  switch(
    type,
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

getTypeSize<-function(type){
  switch(
    type,
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
    return(gpuMagic.option$getDefaultFloat())
  if(typeof(data)=="integer")
    return(gpuMagic.option$getDefaultInt())
  stop("The given type is not defined")
}
convertDataType<-function(data,type){
  checkTypeSupport(type)
  switch(
    type,
    char=rawToChar(as.raw(data)),
    int=as.integer(data),
    as.double(data)
  )
}



#===========================Obtain device infomation==============
#' The function is used to obtain all the opencl-enable devices
#' @export
getDeviceList=function(){
  .C("getDeviceList")
  invisible()
}
#' Get the ith device information, call 'getDeviceList()' first to figure out the index before use this function
#' @param i numeric The device index
#' @export
getDeviceInfo=function(i){
  .C("getDeviceInfo",as.integer(i))
  invisible()
}
#' Get the device detailed information, call 'getDeviceList()' first to figure out the index before use this function
#' @param i The device index
#' @export
getDeviceDetail=function(i){
  .C("getDeviceDetail",as.integer(i))
  invisible()
}
#' Get the current used device
#' @export
getCurDevice=function(){
  .C("getCurDevice")
  invisible()
}
#' Set which device will be used in the opencl, call 'getDeviceList()' first to figure out the index before use this function
#' @param i numeric The device index
#' @export
setDevice=function(i){
  .gpuResourcesManager$releaseAll()
  .C("setDevice",as.integer(i))
  invisible()
}

getCurDeviceIndex<-function(){
  id=-1L
  res=.C("getCurDeviceIndex",id)
  id=res[[1]]
  if(id==-1) 
    setDevice(0)
  else 
    return(id)
  res=.C("getCurDeviceIndex",id)
  id=res[[1]]
  if(id==-1) stop("An error has occured during the device initialization.")
  id
}