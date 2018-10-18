#' @import hash hash
#' @importFrom Rcpp sourceCpp
#' @useDynLib gpuMagic


#======================Type definition====================
T_auto=1L
T_F32=2L
T_F64=3L
T_I32=4L
T_I64=5L
T_DEFAULT=T_F64
T_DEFAULT_INT=T_I32
T_DEFAULT_float=T_F64
getTypeNum<-function(type){
  switch(
    type,
    auto=T_auto,
    float=T_F32,
    double=T_F64,
    integer=T_I32,
    long=T_I64)
}
getTypeStr<-function(type){
  switch(
    type,"auto","float","double","integer","long"
  )
}
getTypeCXXStr<-function(type){
  if(is.character(type))
    type=getTypeNum(type)
  switch(
    type,stop("Auto type is not supported"),
    "float","double",
    "int","long"
  )
}

getTypeSize<-function(type){
  if(is.numeric(type)) type=getTypeStr(type)
  switch(
    type,
    auto=stop("Auto type is not allowed."),
    float=4,
    double=8,
    integer=4,
    long=8
    )
}
getDataType<-function(data){
  if(typeof(data)=="double"||typeof(data)=="numeric")
    return(getTypeStr(T_F64))
  if(typeof(data)=="integer")
    return(getTypeStr(T_I32))
  
  stop("The given type is not defined")
}
convertDataType<-function(data,type){
  switch(
    type,
    stop("Auto type is not allowed."),
    float=as.double(data),
    double=as.double(data),
    integer=as.integer(data),
    long=as.double(data)
  )
}

#======================GPU address S5 class====================

gpuRefAddress=setRefClass("gpuRefAddress", fields = c("address","dim","type","isReady"))
gpuRefAddress$methods(
  initialize = function(data,type) {
    data=as.matrix(data)
    .self$dim=dim(data)
    .self$type=type
    .self$address=.gpuResourcesManager$upload(data,type)
    .self$isReady=TRUE
  }
  )
gpuRefAddress$methods(
  getAddress = function() {
    .gpuResourcesManager$getAddress(.self$address)
  }
)
gpuRefAddress$methods(
  finalize = function() {
    #message("I did something")
    .gpuResourcesManager$releaseAddress(.self$address)
  }
)
gpuRefAddress$methods(
  upload = function(data,type) {
    .gpuResourcesManager$releaseAddress(.self$address)
    .self$initialize(data,type)
    .self$isReady=TRUE
  }
)
gpuRefAddress$methods(
  download = function() {
    .gpuResourcesManager$download(.self$address,.self$dim,.self$type)
  }
)
gpuRefAddress$methods(
  getReadyStatus = function() {
    .self$isReady
  }
)
gpuRefAddress$methods(
  setReadyStatus = function(status) {
    .self$isReady=status
  }
)


#======================GPU resources manager====================
#The gpu manager is not supposed to be called by the user
#It manage the all resources on a device
#The data will be automatically released when the it is not in use
.gpuResourcesManager<-local({
  e=new.env()
  e$unload=FALSE
  e$totalMemory=10^9
  #e$totalMemory=12
  e$memoryUsage=0
  e$maxAddressNum=.Machine$integer.max
  #e$maxAddressNum=10
  e$addressList=hash()
  e$addressSizeList=hash()
  e$empPtr=0
  list(
    upload=function(data,type){
      ##Check if the data is larger than the available memory size
      size=getTypeSize(type)*dim(data)[1]*dim(data)[2]
      if(e$memoryUsage+size>e$totalMemory){
        message("The data is larger than the available GPU memory, a garbage collection is triggered")
        gc()
        if(e$memoryUsage+size>e$totalMemory)
          stop("Garbage collection failed, no memory can be released")
        }
      e$memoryUsage=e$memoryUsage+size
      
      
      ##Check if there is at least one avaible memory index
      if(length(e$addressList)>=e$maxAddressNum){
        message("The GPU address index is full, a garbage collection is triggered")
        gc()
        if(length(e$addressList)>=e$maxAddressNum){
          stop("Garbage collection failed, no address can be released")
        }
      }
      
      ##Find an empty index
      repeat{
        if(e$empPtr>=e$maxAddressNum){
          e$empPtr=0
        }
        e$empPtr=e$empPtr+1
        if(!has.key(as.character(e$empPtr),e$addressList))
          break
      }
      
      
      res=.C(
        "upload",convertDataType(data,type),
        as.double(dim(data)),getTypeNum(type),as.double(0)
      )
      e$addressList[[as.character(e$empPtr)]]=res[[length(res)]]
      e$addressSizeList[[as.character(e$empPtr)]]=size
      
      #e$addressList[[as.character(e$empPtr)]]=1
      return(e$empPtr)
    },
    download=function(ind,dim,type){
      if(!has.key(as.character(ind),e$addressList))
        stop("The GPU resources does not exist!")
      ad=e$addressList[[as.character(ind)]]
      len=dim[1]*dim[2]
      empData=convertDataType(rep(0,len),type)
      res=.C("download",empData,getTypeNum(type),ad)
      
      matrix(res[[1]],dim[1],dim[2])
    },
    getAddress=function(ind){
      return(e$addressList[[as.character(ind)]])
    },
    releaseAddress=function(ind){
      if(e$unload)
        return()
      #message(ind)
      if(!hash::has.key(as.character(ind),e$addressList)){
        warning("GPU memory has already been free")
        return()
      }
      .C("clear",e$addressList[[as.character(ind)]])
      e$memoryUsage=e$memoryUsage-e$addressSizeList[[as.character(ind)]]
      del(as.character(ind),e$addressList)
      del(as.character(ind),e$addressSizeList)
    },
    releaseAll=function(){
      if(length(e$addressList)!=0)
        warning("The function releaseAll may cause an incorrect release of the GPU memory.\nPlease clear the global environment before you release them.")
      
      for(i in keys(e$addressList)){
        .C("clear",e$addressList[[i]])
      }
      clear(e$addressList)
      clear(e$addressSizeList)
      e$memoryUsage=0
      gc()
      invisible()
    },
    getGPUusage=function(){
      if(e$totalMemory>10^7){
      message(paste0("Max GPU memory: ",ceiling((e$totalMemory)/1024/1024),"MB"))
      message(paste0("Current GPU usage: ",ceiling((e$memoryUsage)/1024/1024),"MB(",ceiling(e$memoryUsage/e$totalMemory*100),"%)"))
      }else{
        if(e$totalMemory>10^4){
        message(paste0("Max GPU memory: ",ceiling((e$totalMemory)/1024),"KB"))
        message(paste0("Current GPU usage: ",ceiling((e$memoryUsage)/1024),"KB(",ceiling(e$memoryUsage/e$totalMemory*100),"%)"))
        }else{
          message(paste0("Max GPU memory: ",e$totalMemory,"Byte"))
          message(paste0("Current GPU usage: ",e$memoryUsage,"Byte(",ceiling(e$memoryUsage/e$totalMemory*100),"%)"))
        }
      }
      message(paste0("Max Memory container length: ",e$maxAddressNum))
      message(paste0("Current container length: ",length(e$addressList)))  
    },
    setMaxMemLimit=function(mem=0){
      if(mem==0) mem=10^9
      tmp=e$totalMemory
      e$totalMemory=mem
      tmp
    },
    deleteEnv=function(){
      rm(list =ls(envir = e),envir=e)
      e$unload=TRUE
    }
  )
})


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



