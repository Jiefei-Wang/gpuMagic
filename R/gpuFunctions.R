
checkTypeSupport<-function(type){
  if(!(type %in% gpuMagic.option$getAvailableType()))
    stop("The variable type ", type," is not supported")
}
getTypeNum<-function(type){
  switch(
    type,
    char=1,
    half=2,
    float=3,
    double=4,
    int=5,
    long=6,
    uint=7,
    ulong=8,
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
    char=1,
    half=2,
    float=4,
    double=8,
    int=4,
    long=8,
    uint=4,
    ulong=8,
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
    integer=as.integer(data),
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
  .C("getCurDeviceIndex",id)
  if(id==-1) stop("An error has occured during the device initialization.")
  id
}