#' @include pkgFunc.R
#======================GPU resources manager====================
#The gpu manager is not supposed to be called by the user
#It manage the all resources on a device
#The data will be automatically released when the it is not in use
#' @export
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
  
  getGPUmemInd<-function(size){
    if(e$memoryUsage+size>e$totalMemory){
      if(DEBUG)
        message("The data is larger than the available GPU memory, a garbage collection is triggered")
      gc()
      if(e$memoryUsage+size>e$totalMemory)
        stop("The data is larger than the available GPU memory! Garbage collection can not free more space")
    }
    e$memoryUsage=e$memoryUsage+size
    
    
    ##Check if there is at least one avaible memory index
    if(length(e$addressList)>=e$maxAddressNum){
      if(debug)
        message("The GPU address index is full, a garbage collection is triggered")
      gc()
      if(length(e$addressList)>=e$maxAddressNum){
        stop("The GPU address index is full! Garbage collection can not free more index")
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
    return(e$empPtr)
  }
  
  list(
    upload=function(data,type){
      ##Check if the data is larger than the available memory size and get the memory index
      size=getTypeSize(type)*length(data)
      gpuInd=getGPUmemInd(size)
      
        res=.C(
          "upload",convertDataType(data,type),
          as.double(length(data)),getTypeNum(type),as.double(0)
        )
      
      e$addressList[[as.character(gpuInd)]]=res[[length(res)]]
      e$addressSizeList[[as.character(gpuInd)]]=size
      
      return(gpuInd)
    },
    gpuMalloc<-function(len,type){
      ##Check if the data is larger than the available memory size and get the memory index
      size=getTypeSize(type)*len
      gpuInd=getGPUmemInd(size)
      
      res=.C(
        "gpuMalloc",as.double(len),getTypeNum(type),as.double(0)
      )
      
      e$addressList[[as.character(gpuInd)]]=res[[length(res)]]
      e$addressSizeList[[as.character(gpuInd)]]=size
      
      return(gpuInd)
    }
    ,
    download=function(ind,len,type){
      if(!has.key(as.character(ind),e$addressList))
        stop("The GPU resources does not exist!")
      ad=e$addressList[[as.character(ind)]]
      if(type=="char"){
        empData=paste0(rep(" ",len),collapse = "")
        res=.C("download",empData,ad)
        return(as.numeric(charToRaw(res[[1]])))
      }else{
        empData=convertDataType(rep(0,len),type)
        res=.C("download",empData,ad)
        
        return(res[[1]])
      }
      
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
        #message(i)
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





