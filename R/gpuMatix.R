
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
  ad$gpuMalloc(row*col,type)
  
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
