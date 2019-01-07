#' @include pkgFunc.R
#======================GPU resources manager====================
#The gpu manager is not supposed to be called by the user
#It manage the all resources on a device
#The data will be automatically released when the it is not in use
#' @export
.gpuResourcesManager<-local({
  internalVars=new.env()
  internalVars$unload=FALSE
  internalVars$totalMemory=hash()
  internalVars$GCcutoff=0.9
  internalVars$memoryUsage=hash()
  internalVars$addressList=hash()
  internalVars$addressSizeList=hash()
  internalVars$empPtr=0
  
  
  globalVars=new.env()
  globalVars$deviceInfo=c()
  globalVars$curDevice=hash()
  globalVars$curDevice[["1"]]=c(0,0)
  
  
  
  
  checkGPUmemUsage<-function(devKey,size){
    if(length(keys(internalVars$memoryUsage))==0)
      setDevice(keys(globalVars$curDevice))
    if(internalVars$memoryUsage[[devKey]]+size>internalVars$totalMemory[[devKey]]*internalVars$GCcutoff){
      if(DEBUG)
        message("A garbage collection is triggered to release the GPU memory")
      gc()
      if(internalVars$memoryUsage[[devKey]]+size>internalVars$totalMemory[[devKey]])
        stop("The data is larger than the available GPU memory! Garbage collection can not free more space")
    }
    internalVars$memoryUsage[[devKey]]=internalVars$memoryUsage[[devKey]]+size
  }
  
  
  ##########################################################################################
  
  list(
    upload=function(deviceId,data,type){
      curDevice=getSelectedDevice(deviceId)
      devKey=as.character(deviceId)
      ##Check if the data is larger than the available memory size and get the memory index
      size=as.double(getTypeSize(type))*length(data)
      checkGPUmemUsage(devKey,size)
      
      res=.C(
        "upload",curDevice[1],curDevice[2],convertDataType(data,type),
        as.double(length(data)),getTypeNum(type),as.double(0)
      )
      
      gpuAd=res[[length(res)]]
      adKey=digest(gpuAd)
      internalVars$addressSizeList[[devKey]][[adKey]]=size
      internalVars$addressList[[devKey]][[adKey]]=gpuAd
      return(gpuAd)
    },
    gpuMalloc=function(deviceId,len,type){
      updateDeviceInfo(initialOnly=T)
      curDevice=getSelectedDevice(deviceId)
      devKey=as.character(deviceId)
      
      size=getTypeSize(type)*len
      ##Check if the data is larger than the available memory size
      checkGPUmemUsage(devKey,size)
      
      res=.C(
        "gpuMalloc",curDevice[1],curDevice[2],as.double(len),getTypeNum(type),as.double(0)
      )
      
      gpuAd=res[[length(res)]]
      adKey=digest(gpuAd)
      internalVars$addressSizeList[[devKey]][[adKey]]=size
      internalVars$addressList[[devKey]][[adKey]]=gpuAd
      
      return(gpuAd)
    }
    ,
    download=function(deviceId,gpuAd,len,type){
      curDevice=getSelectedDevice(deviceId)
      devKey=as.character(deviceId)
      adKey=digest(gpuAd)
      if(!has.key(adKey,internalVars$addressSizeList[[devKey]]))
        stop("The GPU resources does not exist!")
      
      #General case
      empData=convertDataType(rep(0,len),type)
      res=.C("download",empData,gpuAd)
      return(as.numeric(res[[1]]))
    },
    releaseAddress=function(deviceId,gpuAd){
      curDevice=getSelectedDevice(deviceId,checkInital = F)
      devKey=as.character(deviceId)
      adKey=digest(gpuAd)
      if(internalVars$unload)
        return()
      if(!hash::has.key(adKey,internalVars$addressSizeList[[devKey]])){
        return()
      }
      .C("release",internalVars$addressList[[devKey]][[adKey]])
      
      internalVars$memoryUsage[[devKey]]=
        internalVars$memoryUsage[[devKey]]-internalVars$addressSizeList[[devKey]][[adKey]]
      del(adKey,internalVars$addressSizeList[[devKey]])
      del(adKey,internalVars$addressList[[devKey]])
    },
    releaseAll=function(){
      for(i in keys(internalVars$addressSizeList)){
        for(j in keys(internalVars$addressList[[i]])){
          .C("release",internalVars$addressList[[i]][[j]])
        }
        clear(internalVars$addressSizeList[[i]])
        clear(internalVars$addressList[[i]])
      }
      clear(internalVars$memoryUsage)
      gc()
      invisible()
    },
    getGPUusage=function(){
      deviceList=c()
      maxMem=c()
      usedMem=c()
      for(i in keys(internalVars$totalMemory)){
        deviceList=c(deviceList,paste0("Device ",i))
        maxMem=c(maxMem,internalVars$totalMemory[[i]])
        usedMem=c(usedMem,internalVars$memoryUsage[[i]])
      }
      memPercent=paste0("(",ceiling(usedMem/maxMem*100),"%)")
      
      usedMem_char=sapply(usedMem,format_memory_size_output)
      maxMem_char=sapply(maxMem,format_memory_size_output)
      
      usedMem_char=paste0("--Used: ",usedMem_char,memPercent)
      maxMem_char=paste0("--Total: ",maxMem_char)
      
      if(length(deviceList)>1){
      deviceList=StrAlign(deviceList,sep="\\l")
      usedMem_char=StrAlign(usedMem_char,sep="\\l")
      maxMem_char=StrAlign(maxMem_char,sep="\\l")
      }
      
      message("Device memory usage:")
      message(paste(deviceList,usedMem_char,maxMem_char,sep=" ",collapse = "\n"))
    },
    setMaxMemLimit=function(mem=0){
      if(mem==0) mem=10^9
      tmp=internalVars$totalMemory
      internalVars$totalMemory=mem
      tmp
    },
    globalVars=globalVars,
    internalVars=internalVars,
    deleteEnv=function(){
      .gpuResourcesManager$releaseAll()
      rm(list =ls(envir = internalVars),envir=internalVars)
      rm(list =ls(envir = globalVars),envir=globalVars)
      internalVars$unload=TRUE
    }
  )
})




#A tiny function that can make the output more compact
#Auto convert the unit between byte, kb, mb, and gb
#The input is the memory size in byte
format_memory_size_output<-function(x){
  if(x>10^9)
    return(paste0(ceiling((x)/1024^3*100)/100," GB"))
  if(x>10^7)
    return(paste0(ceiling((x)/1024^2*100)/100," MB"))
  if(x>10^4)
    return(paste0(ceiling((x)/1024*100)/100," KB"))
  
  return(paste0(x," Byte"))
}


#This function query the device info and storage the results into the global variable
#If initialOnly=T the function will only update the deviceInfo in the first call
updateDeviceInfo<-function(initialOnly=F){
  if(initialOnly){
    if(!is.null(.gpuResourcesManager$globalVars$deviceInfo))
      return()
  }
  platformNum=getPlatformNum()
  
  deviceInfo=c()
  id=0
  for(i in seq_len(platformNum)-1){
    deviceNum=getDeviceNum(i)
    for(j in seq_len(deviceNum)-1){
      curDeviceInfo=getSingleDeviceInfo(i,j)
      curDeviceInfo$id=id
      deviceInfo=rbind(deviceInfo,curDeviceInfo)
      id=id+1
    }
  }
  
  if(length(deviceInfo)==0){
    message("No device has been found, please make sure the computer has a graphic card or the driver has been properly installed.")
    message("Hint:",
            "\nFor CPU, you can install the intel's / ATI's graphic driver for the intel's / AMD's CPU respectively.",
            "\nFor GPU, you need to download the graphic driver from your vendor's website.")
    return()
  }
  deviceInfo$haslocalMemory=deviceInfo$haslocalMemory==1
  deviceInfo$id=as.integer(deviceInfo$id+1)
  deviceInfo$platform=as.integer(deviceInfo$platform+1)
  deviceInfo$device=as.integer(deviceInfo$device+1)
  .gpuResourcesManager$globalVars$deviceInfo=deviceInfo
}

selectDevice=function(devices){
  .gpuResourcesManager$releaseAll()
  updateDeviceInfo(initialOnly=T)
  deviceInfo=.gpuResourcesManager$globalVars$deviceInfo
  if(range(devices)[1]<=0||range(devices)[2]>nrow(deviceInfo)){
    stop("Invalid device id!")
  }
  
  curDeviceInfo=deviceInfo[devices,,drop=F]
  
  clear(.gpuResourcesManager$internalVars$addressSizeList)
  clear(.gpuResourcesManager$internalVars$addressList)
  clear(.gpuResourcesManager$internalVars$memoryUsage)
  clear(.gpuResourcesManager$internalVars$totalMemory)
  clear(.gpuResourcesManager$globalVars$curDevice)
  
  for(i in seq_len(length(devices))){
    key=as.character(devices[i])
    .gpuResourcesManager$internalVars$addressSizeList[[key]]=hash()
    .gpuResourcesManager$internalVars$addressList[[key]]=hash()
    .gpuResourcesManager$globalVars$curDevice[[key]]=as.integer(curDeviceInfo[i,c("platform","device"),drop=F]-1)
    .gpuResourcesManager$internalVars$totalMemory[[key]]=as.double(curDeviceInfo[i,"globalMemory"])
    .gpuResourcesManager$internalVars$memoryUsage[[key]]=0
  }
}

getFirstSelectedDevice<-function(){
  deviceList=sort(as.integer(keys(.gpuResourcesManager$globalVars$curDevice)))
  as.integer(deviceList[1])
}
#Get the platform and device id
#If checkInitial=T, the function will check if the device has been selected
#If checkInitial=F, the function will just return the device information without check
getSelectedDevice<-function(device,checkInital=T){
  if(checkInital){
    devKey=as.character(device)
    if(!isDeviceSelected(devKey)) 
      stop("The device has not been initialized! Please call setDevice() to initialize the device before use it.")
    
    return(as.integer(.gpuResourcesManager$globalVars$curDevice[[devKey]]))
  }else{
    updateDeviceInfo(initialOnly=T)
    curDevice=as.integer(.gpuResourcesManager$globalVars$deviceInfo[device,c("platform","device")]-1)
    if(is.null(curDevice)) stop("the device does not exist!")
    return(curDevice)
  }
}
isDeviceSelected<-function(device){
  devKey=as.character(device)
  has.key(devKey,.gpuResourcesManager$globalVars$curDevice)
}