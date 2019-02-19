
getPlatformNum <- function() {
  res = .Call(C_getPlatformNum)
  res
}
getDeviceNum <- function(platform) {
  res = .Call(C_getDeviceNum, as.integer(platform))
  res
}
getSingleDeviceInfo <- function(platform, device) {
  deviceInfo = .Call(C_getDeviceInfo, as.integer(platform), as.integer(device))
  names(deviceInfo) = c("deviceName", "deviceType", "globalMemory", "localMemory", 
                        "haslocalMemory", "opencl_version", "compute_unit_num","work_group_size")
  deviceInfo = as.data.frame(deviceInfo, stringsAsFactors = FALSE)
  deviceInfo = cbind(data.frame(id = NA, platform = platform, device = device), 
                     deviceInfo)
  
  deviceInfo$deviceType = switch(as.character(deviceInfo$deviceType), 
                                 `0` = "CPU", `1` = "GPU", `2` = "other")
  
  deviceInfo
}


#Get the thread number from the options
#If the option is auto, the package setting will be used
getThreadNumber<-function(options){
  if (options$localThreadNum == "auto") {
    localThreadNum = as.numeric(gpuMagic.getOptions("default.thread.num")) 
  } else {
    localThreadNum = options$localThreadNum
  }
  return(localThreadNum)
}

# great common divisor
gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}
