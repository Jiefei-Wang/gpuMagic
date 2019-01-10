
.gpuMatrix=setClass(
  Class="gpuMatrix",
  slots = c(data="ANY",dimension="vector",type="character",gpuAddress="ANY",device="numeric")
)
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

setGeneric(name = "upload",def = function(obj) standardGeneric("upload"))

#' @export
setMethod(
  f="upload",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@gpuAddress$upload(.device(obj),.data(obj),.type(obj))
    obj
  }
)


setGeneric(name="download",def=function(obj) standardGeneric("download"))

#' @export
setMethod(
  f="download",
  signature = "gpuMatrix",
  definition = function(obj){
    obj@data=matrix(obj@gpuAddress$download(),.nrow(obj),.ncol(obj))
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
              return(.data(x)[])
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
            
            return(.data(x)[i,j])
          })
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


setGeneric(name="getSize",def=function(obj) standardGeneric("getSize"))

#' @export
setMethod(
  f="getSize",
  signature = "gpuMatrix",
  definition = function(obj){
    .nrow(obj)*.ncol(obj)*getTypeSize(.type(obj))
  }
)