


.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="vector",type="character",gpuAddress="ANY")
)

gpuMatrix<-function(data,type=T_auto){
  if(is.numeric(type))
    type=getTypeStr(as.integer(type))
   if(type=="auto")
     type=getDataType(data)
  ad=gpuRefAddress(data,type)
  obj=.gpuMatrix(data=data,type=type,gpuAddress=ad)
  
  obj
}

.data<-function(obj) obj@data
".data<-"<-function(obj,value){
  obj@data<-value
  obj
}

.type<-function(obj) obj@type
".type<-"<-function(obj,type){
  obj@type<-value
  obj
}



setGeneric(name = "upload",def = function(obj) standardGeneric("upload"))

#' @export
setMethod(
  f="upload",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@gpuAddress$upload(.data(obj),.type(obj))
  }
)


setGeneric(name="download",def=function(obj) standardGeneric("download"))

#' @export
setMethod(
  f="download",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@data=obj@gpuAddress$download()
    
    obj
  }
)






