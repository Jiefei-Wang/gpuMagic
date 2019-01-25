
.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="ANY",dimension="vector",type="character",gpuAddress="ANY",device="numeric")
)


#' gpuMatrix class
#' 
#' @details `gpuMatrix()`: Create a matrix in an openCL device
#' 
#' @param data It can be a matrix or an R object that can be converted into a matrix.
#' @param type The precision that is used to store the data, the default is `gpuMagic.getOptions("default.float")`.
#' @param device The device that the data is sent to, the default is the first device.
#' 
#' @examples
#' n=10
#' m=20
#' A=matrix(runif(n*m),n,m)
#' #Create a 64 bit floating point GPU matrix
#' A_dev=gpuMatrix(A,"double")
#' 
#' #Create an empty matrix
#' B_dev=gpuEmptMatrix(row=n,col=m)
#' @export
gpuMatrix<-function(data,type="auto",device="auto"){
  data=as.matrix(data)
  
  if(type=="auto"){
    type=GPUVar$default_float
  }
  if(device=="auto"){
    device=getFirstSelectedDevice()
  }
  if(length(device)>1)
    stop("Only one device is supported")
  
  #check if the device has been initialized
  getSelectedDevice(device)
   
  checkTypeSupport(type)
  ad=gpuRefAddress()
  ad$upload(device,data,type)
  
  obj=.gpuMatrix(data=data,dimension=dim(data),type=type,gpuAddress=ad,device=device)
  
  
  obj
}

 
#' @details `gpuEmptMatrix()`: Create an empty matrix without initialization in an openCL device
#' @inheritParams gpuMatrix
#' @param row,col the row and column number of the matrix
#' @rdname gpuMatrix
#' @export
gpuEmptMatrix<-function(row=1,col=1,type="auto",device="auto"){
  if(type=="auto"){
    type=GPUVar$default_float
  }
  if(device=="auto"){
    device=getFirstSelectedDevice()
  }
  if(length(device)>1)
    stop("Only one device is supported")
  #check if the device has been initialized
  getSelectedDevice(device)
  
  checkTypeSupport(type)
  ad=gpuRefAddress()
  len=max(row*col,1)
  ad$gpuMalloc(device,len,type)
  
  obj=.gpuMatrix(data=NULL,dimension=c(row,col),type=type,gpuAddress=ad,device=device)
  
  
  obj
}



#======================Get the slot data======================
.getAddress<-function(obj){
  ad=obj@gpuAddress$getAddress()
  if(is.null(ad))
    stop("The GPU address does not exist")
  ad
}
.device<-function(obj){
  obj@device
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

".dim<-"<-function(obj,value) {
  obj@dimension=value
  obj
}
".nrow<-"<-function(obj,value){
  obj@dimension[1]=value
  obj
}
".ncol<-"<-function(obj,value){
  obj@dimension[2]=value
  obj
}



.type<-function(obj) obj@type
".type<-"<-function(obj,value){
  checkTypeSupport(value)
  obj@type<-value
  obj
}

#======================Functions======================

#' @details `upload()`: The function will automatically be called when an gpuMatrix object is created.
#' It is only needed when you want to update value of the matrix.
#' 
#' @aliases upload,gpuMatrix-method upload
#' @rdname gpuMatrix
#' @param x an gpuMatrix object
setGeneric(name = "upload",def = function(x) standardGeneric("upload"))


#' @export
setMethod(
  f="upload",
  signature = "gpuMatrix",
  definition = function(x){
    x@gpuAddress$upload(.device(x),.data(x),.type(x))
    x
  }
)

#' @details `download()`: Get the data from the device. You should explicitly call it when you want to collect the data from the device.
#' 
#' @inheritParams upload
#' @aliases download,gpuMatrix-method download
#' @rdname gpuMatrix
setGeneric(name="download",def=function(x) standardGeneric("download"))


#' @export
setMethod(
  f="download",
  signature = "gpuMatrix",
  definition = function(x){
    x@data=x@gpuAddress$download()
    .Call(C_asMatrix,x@data,as.integer(.dim(x)))
    x
  }
)

#======================General functions overload================


#' @details `nrow()`,`ncol()`: return the number of rows or columns present in `x`
#' 
#' @inheritParams upload
#' @aliases nrow,gpuMatrix-method nrow
#' @rdname gpuMatrix
setGeneric(name="nrow",def=function(x) standardGeneric("nrow"))
#' @export
setMethod("nrow", signature(x="gpuMatrix"),
          function(x) {
            .nrow(x)
          }
)


#' @inherit nrow
#' @aliases ncol,gpuMatrix-method ncol
#' @export
setMethod("ncol", signature(x="gpuMatrix"),
          function(x) {
            .ncol(x)
          }
)

#' @details  `dim()`: Retrieve the dimension of an gpuMatrix object
#' 
#' 
#' @inheritParams upload
#' @aliases dim,gpuMatrix-method dim
#' @rdname gpuMatrix
#' @export
setMethod("dim", signature(x="gpuMatrix"),
          function(x) {
            .dim(x)
          }
)

#' @details `length()`: Get the length of an gpuMatrix object. 
#'
#' @inheritParams upload
#' @name length
#' @aliases length,gpuMatrix-method length
#' @rdname gpuMatrix
#' @export
setMethod("length", signature(x="gpuMatrix"),
          function(x) {
            .length(x)
          }
)
#' Convert the gpuMatrix object into a matrix
#' 
#' The function will convert the gpuMatrix object into a matrix, 
#' if you have run any GPU functions on the gpuMatrix object, 
#' please call `download(x)` to synchronize the data before calling this function. 
#' 
#' @inheritParams upload
#' @param ... This argument is only for compatibility. It does not take any effect.
#' @export
as.matrix.gpuMatrix<-function(x,...){
  as.matrix(.data(x),.nrow(x),.ncol(x))
}
#' Convert the gpuMatrix object into a vector
#' 
#' The function will convert the gpuMatrix object into a vector, 
#' if you have run any GPU functions on the gpuMatrix object, 
#' please call `download(x)` to synchronize the data before calling this function. 
#' 
#' @inheritParams upload
#' @param mode This argument is only for compatibility. It does not take any effect.
#' @export
as.vector.gpuMatrix<-function(x, mode=NULL){
  as.vector(.data(x))
}
getIndexFromExp<-function(Exp){
  requiredFile=c("i","j","drop")
  res=list(i=NA,j=NA,drop=TRUE)
  argList=as.list(Exp)[-c(1,2)]
  argName=names(argList)
  if(!is.null(argName)){
    for(name in argName[argName!=""]){
        res[[name]]=argList[[name]]
        argList[[name]]=NULL
    }
    requiredFile=requiredFile[!(requiredFile%in%argName)]
  }
  if(length(requiredFile)>0&&length(argList)>0){
    for(i in 1:min(length(requiredFile),length(argList))){
      res[[requiredFile[i]]]=deparse(argList[[i]])
    }
  }
  return(res)
}
#' extract/set parts of the data in gpuMatrix object
#'
#' @inheritParams upload
#' @param i,j indices specifying elements to extract or replace. The index j can be missing or empty.
#' @param ... This argument is only for compatibility. It does not take any effect.
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension.
#' @family Extract
#' @rdname extract-methods
#' @docType methods
#' @aliases [,gpuMatrix-method [
#' @rdname gpuMatrix
#' @export
setMethod("[",
          signature(x = "gpuMatrix", i = "ANY", j = "ANY", drop="missing"),
          function(x, i=NA, j=NA,..., drop=TRUE) {
            func_call=sys.call()
            index=getIndexFromExp(func_call)
            #Empty index
            if(
              (index$i==""&&index$j=="")||
              (index$i==""&&is.na(index$j))||
              (is.na(index$i)&&index$j=="")
               )
              return(.data(x)[drop=drop])
            #One index
            if(is.na(index$j))
              return(.data(x)[i,drop=drop])
            if(is.na(index$i))
              stop("Undefined behavior")
            #Two index
            if(index$i=="")
              return(.data(x)[,j,drop=drop])
            if(index$j=="")
              return(.data(x)[i,,drop=drop])
            
            return(.data(x)[i,j,drop=drop])
          })
#' @param value The value you want to set
#' @family Extract
#' @rdname extract-methods
#' @export
setMethod("[<-",
          signature(x = "gpuMatrix", i = "ANY", j = "ANY", value = "numeric"),
          function(x, i, j,..., value) {
            func_call=sys.call()
            index=getIndexFromExp(func_call)
            mydata=.data(x)
            if(
              (index$i==""&&index$j=="")||
              (index$i==""&&is.na(index$j))||
              (is.na(index$i)&&index$j=="")
            ){
              mydata[]<-value
              .data(x)=mydata
              .dim(x)=dim(mydata)
              return(x)
            }
            #One index
            if(is.na(index$j)){
              mydata[i]<-value
              .data(x)=mydata
              .dim(x)=dim(mydata)
              return(x)
              }
            if(is.na(index$i))
              stop("Undefined behavior")
            #Two index
            if(index$i==""){
              mydata[,j]<-value
              .data(x)=mydata
              .dim(x)=dim(mydata)
              return(x)
            }
            if(index$j==""){
              mydata[i,]<-value
              .data(x)=mydata
              .dim(x)=dim(mydata)
              return(x)
            }
            
            mydata[i,j]<-value
            .data(x)=mydata
            .dim(x)=dim(mydata)
            return(x)
              
          })


#' @details Get the matrix size in byte
#' 
#' 
#' @aliases getSize,gpuMatrix-method getSize
#' @rdname gpuMatrix
#' @inheritParams upload
setGeneric(name="getSize",def=function(x) standardGeneric("getSize"))


#' @export
setMethod(
  f="getSize",
  signature = "gpuMatrix",
  definition = function(x){
    .nrow(x)*.ncol(x)*getTypeSize(.type(x))
  }
)