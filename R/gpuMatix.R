
.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="vector",type="character",isReady="logical",gpuAddress="ANY")
)

gpuMatrix<-function(data,type=T_auto){
  if(is.numeric(type))
    type=getTypeStr(as.integer(type))
   if(type=="auto")
     type=getDataType(data)
  ad=gpuRefAddress(data,type)
  obj=.gpuMatrix(data=data,type=type,isReady=TRUE,gpuAddress=ad)
  
  obj
}
.getAddress<-function(obj){
  ad=obj@gpuAddress$getAddress()
  if(is.null(ad))
    stop("The GPU address does not exist")
  ad
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
.readyStatus<-function(obj) obj@isReady
".readyStatus<-"<-function(obj,status){
  obj@isReady<-status
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

setGeneric(name="sync",def=function(obj) standardGeneric("sync"))

#' @export
setMethod(
  f="sync",
  signature = "gpuMatrix",
  definition = function(obj){
    .data(obj)=obj@gpuAddress$download()
    if(!.readyStatus(obj)){
      upload(obj)
    }
    if(!obj@gpuAddress$getReadyStatus()){
      obj=download(obj)
    }
    
    obj
  }
)




