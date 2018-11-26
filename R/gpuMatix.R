
.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="matrix",dimension="vector",type="character",gpuAddress="ANY")
)
#' @export
gpuMatrix<-function(data,type="auto"){
  data=as.matrix(data)
  
  if(type=="auto"){
    type=gpuMagic.option$getDefaultType()
  }
   
  checkTypeSupport(type)
  ad=gpuRefAddress()
  ad$upload(data,type)
  
  obj=.gpuMatrix(data=data,dimension=dim(data),type=type,gpuAddress=ad)
  
  
  obj
}
#' @export
gpuEmptMatrix<-function(row=1,col=1,type="auto"){
  if(type=="auto"){
    type=gpuMagic.option$getDefaultType()
  }
  
  checkTypeSupport(type)
  ad=gpuRefAddress()
  ad$upload(row*col,type)
  
  obj=.gpuMatrix(data=NULL,dimension=c(row,col),type=type,gpuAddress=ad)
  
  
  obj
}



#======================Get the slot data======================
.getAddress<-function(obj){
  ad=obj@gpuAddress$getAddress()
  if(is.null(ad))
    stop("The GPU address does not exist")
  ad
}

.data<-function(obj){
  if(is.null( obj@data))
    stop("The data is not available")
  obj@data
}
".data<-"<-function(obj,value){
  obj@data<-as.matrix(value)
  obj
}
.dim<-function(obj) obj@dimension
.nrow<-function(obj)obj@dimension[1]
.ncol<-function(obj)obj@dimension[2]
.length<-function(obj) obj@dimension[1]*obj@dimension[2]


.type<-function(obj) obj@type
".type<-"<-function(obj,value){
  checkTypeSupport(value)
  obj@type<-value
  obj
}

#======================Functions======================

setGeneric(name = "upload",def = function(obj) standardGeneric("upload"))

#' @export
setMethod(
  f="upload",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@gpuAddress$upload(.data(obj),.type(obj))
    obj
  }
)


setGeneric(name="download",def=function(obj) standardGeneric("download"))

#' @export
setMethod(
  f="download",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@data=as.matrix(obj@gpuAddress$download(),.nrow(obj),.ncol(obj))
    obj
  }
)

#======================General functions overload================


#' @export
setMethod("nrow", signature(x="gpuMatrix"),
          function(x) {
            .nrow(x)
          }
)
#' @export
setMethod("ncol", signature(x="gpuMatrix"),
          function(x) {
            .ncol(x)
          }
)
#' @export
setMethod("dim", signature(x="gpuMatrix"),
          function(x) {
            .dim(x)
          }
)
#' @export
setMethod("length", signature(x="gpuMatrix"),
          function(x) {
            .length(x)
          }
)
#' @export
as.matrix.gpuMatrix<-function(obj,...){
  as.matrix(.data(obj),.nrow(obj),.ncol(obj))
}
#' @export
as.vector.gpuMatrix<-function(obj,...){
  as.vector(.data(obj))
}
#' @export
setMethod("[",
          signature(x = "gpuMatrix", i = "ANY", j = "ANY", drop="missing"),
          function(x, i, j, drop) {
            #message(list(sys = sys.call(), match = match.call()))
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
            #message(list(sys = sys.call(), match = match.call()))
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
            return(x)
          })
