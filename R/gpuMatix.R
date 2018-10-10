
.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="matrix",type="character",isReady="logical",gpuAddress="ANY")
)

gpuMatrix<-function(data,type=T_auto){
  data=as.matrix(data)
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
  obj@data<-as.matrix(value)
  obj
}

.type<-function(obj) obj@type
".type<-"<-function(obj,value){
  obj@type<-value
  obj
}
.readyStatus<-function(obj) obj@isReady
".readyStatus<-"<-function(obj,value){
  obj@isReady<-value
  obj
}



setGeneric(name = "upload",def = function(obj) standardGeneric("upload"))

#' @export
setMethod(
  f="upload",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@gpuAddress$upload(.data(obj),.type(obj))
    .readyStatus(obj)=TRUE
    obj
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
    if(!.readyStatus(obj)){
      obj=upload(obj)
      return(obj)
    }
    if(!obj@gpuAddress$getReadyStatus()){
      obj=download(obj)
      obj@gpuAddress$setReadyStatus(TRUE)
      return(obj)
    }
  }
)

#======================General functions overload================
#' @export
setMethod("dim", signature(x="gpuMatrix"),
          function(x) {
            dim(.data(x))
          }
)

#' @export
as.matrix.gpuMatrix<-function(obj,...){
  as.matrix(.data(obj))
}
#' @export
as.vector.gpuMatrix<-function(obj,...){
  as.vector(.data(obj))
}
#' @export
setMethod("[",
          signature(x = "gpuMatrix", i = "ANY", j = "ANY", drop="missing"),
          function(x, i, j, drop) {
            message(nargs())
            if(missing(i)&&missing(j))
              return(.data(x))
            if(missing(i))
              return(x@data[,j,drop=drop])
            if(missing(j))
              return(x@data[i,,drop=drop])
            return(.data(x)[i,j])
          })
#' @export
setMethod("[<-",
          signature(x = "gpuMatrix", i = "ANY", j = "ANY", value = "numeric"),
          function(x, i, j, value) {
            if(missing(i)&&missing(j))
              .data(x)=value
            else{
              if(missing(i))
                x@data[,j]=value
              else{
                if(missing(j))
                  x@data[i,]=value
                else
                  x@data[i,j]=value
              }
            }
            .readyStatus(x)<-FALSE
            return(x)
          })
