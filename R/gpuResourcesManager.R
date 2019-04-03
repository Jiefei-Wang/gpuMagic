#' @include pkgFunc.R
#' @include hash.R
# ======================GPU resources manager==================== The
# gpu manager is not supposed to be called by the user It manage the
# all resources on a device The data will be automatically released
# when the it is not in use
.gpuResourcesManager <- local({
    internalVars = new.env()
    internalVars$unload = FALSE
    internalVars$totalMemory = hash()
    internalVars$GCcutoff = 0.7
    internalVars$memoryUsage = hash()
    internalVars$addressList = hash()
    internalVars$addressSizeList = hash()
    internalVars$empPtr = 0
    
    
    globalVars = new.env()
    globalVars$deviceInfo = c()
    globalVars$curDevice = hash()
    globalVars$curDevice[["1"]] = c(0, 0)
    
    
    
    
    checkGPUmemUsage <- function(devKey, size) {
        if (length(keys(internalVars$memoryUsage)) == 0) 
            setDevice(keys(globalVars$curDevice))
        
        if (internalVars$memoryUsage[[devKey]] + size > internalVars$totalMemory[[devKey]] * 
            internalVars$GCcutoff) {
            if (DEBUG) 
                message("A garbage collection is triggered to release the GPU memory")
            gc()
            if (internalVars$memoryUsage[[devKey]] + size > internalVars$totalMemory[[devKey]]) 
                stop("The data is larger than the available GPU memory! Garbage collection can not free more space")
        }
        internalVars$memoryUsage[[devKey]] = internalVars$memoryUsage[[devKey]] + 
            size
    }
    
    
    ########################################################################################## 
    
    list(upload = function(deviceId, data, type) {
        curDevice = getSelectedDevice(deviceId)
        devKey = as.character(deviceId)
        ## Check if the data is larger than the available memory size and get
        ## the memory index
        size = as.double(getTypeSize(type)) * length(data)
        checkGPUmemUsage(devKey, size)
        
        gpuAd = .Call(C_upload, curDevice[1], curDevice[2], data, as.double(length(data)), 
            getTypeNum(type))
        
        adKey = digest(getTrueAd(gpuAd))
        
        internalVars$addressSizeList[[devKey]][[adKey]] = size
        internalVars$addressList[[devKey]][[adKey]] = gpuAd
        return(gpuAd)
    }, gpuMalloc = function(deviceId, len, type) {
        updateDeviceInfo(initialOnly = TRUE)
        curDevice = getSelectedDevice(deviceId)
        devKey = as.character(deviceId)
        
        size = getTypeSize(type) * len
        ## Check if the data is larger than the available memory size
        checkGPUmemUsage(devKey, size)
        
        gpuAd = .Call(C_gpuMalloc, curDevice[1], curDevice[2], as.double(len), 
            getTypeNum(type))
        
        adKey = digest(getTrueAd(gpuAd))
        internalVars$addressSizeList[[devKey]][[adKey]] = size
        internalVars$addressList[[devKey]][[adKey]] = gpuAd
        
        return(gpuAd)
    }, download = function(deviceId, gpuAd) {
        curDevice = getSelectedDevice(deviceId)
        devKey = as.character(deviceId)
        adKey = digest(getTrueAd(gpuAd))
        if (!has.key(adKey, internalVars$addressSizeList[[devKey]])) stop("The GPU resources does not exist!")
        
        # General case
        res = .Call(C_download, gpuAd)
        return(res)
    }, releaseAddress = function(deviceId, gpuAd) {
        curDevice = getSelectedDevice(deviceId, checkInital = FALSE)
        devKey = as.character(deviceId)
        adKey = digest(getTrueAd(gpuAd))
        if (internalVars$unload) return()
        if (!has.key(adKey, internalVars$addressSizeList[[devKey]])) {
            return()
        }
        .Call(C_release, gpuAd)
        internalVars$memoryUsage[[devKey]] = internalVars$memoryUsage[[devKey]] - 
            internalVars$addressSizeList[[devKey]][[adKey]]
        
        del(adKey, internalVars$addressSizeList[[devKey]])
        del(adKey, internalVars$addressList[[devKey]])
    }, releaseAll = function() {
        for (dev in keys(internalVars$addressList)) {
            for (ad in values(internalVars$addressList[[dev]])) {
                .gpuResourcesManager$releaseAddress(dev, ad)
            }
        }
        gc()
        invisible()
    }, getGPUusage = function() {
      deviceKey=keys(internalVars$totalMemory)
      nKey=length(deviceKey)
      deviceList = vector("character",nKey)
      maxMem = vector("numeric",nKey)
      usedMem = vector("numeric",nKey)
      for (i in seq_along(deviceKey)) {
        deviceList[i] = paste0("Device ", deviceKey[i])
        maxMem[i] =internalVars$totalMemory[[deviceKey[i]]]
        usedMem[i] = internalVars$memoryUsage[[deviceKey[i]]]
      }
        memPercent = paste0("(", ceiling(usedMem/maxMem * 100), "%)")
        
        usedMem_char = vapply(usedMem, format_memory_size_output,character(1))
        maxMem_char = vapply(maxMem, format_memory_size_output,character(1))
        
        usedMem_char = paste0("--Used: ", usedMem_char, memPercent)
        maxMem_char = paste0("--Total: ", maxMem_char)
        
        if (length(deviceList) > 1) {
            deviceList = StrAlign(deviceList, sep = "\\l")
            usedMem_char = StrAlign(usedMem_char, sep = "\\l")
            maxMem_char = StrAlign(maxMem_char, sep = "\\l")
        }
        
        message("Device memory usage:")
        message(paste(deviceList, usedMem_char, maxMem_char, sep = " ", 
            collapse = "\n"))
    }, setMaxMemLimit = function(device, mem) {
        device = as.character(device)
        if (!has.key(device, internalVars$totalMemory)) stop("The device has not been initialized")
        tmp = internalVars$totalMemory[[device]]
        internalVars$totalMemory[[device]] = mem
        tmp
    }, globalVars = globalVars, internalVars = internalVars, deleteEnv = function() {
        .gpuResourcesManager$releaseAll()
        rm(list = ls(envir = internalVars), envir = internalVars)
        rm(list = ls(envir = globalVars), envir = globalVars)
        internalVars$unload = TRUE
    })
})




# A tiny function that can make the output more compact Auto convert
# the unit between byte, kb, mb, and gb The input is the memory size in
# byte
format_memory_size_output <- function(x) {
    if (x > 10^9) 
        return(paste0(ceiling((x)/1024^3 * 100)/100, " GB"))
    if (x > 10^7) 
        return(paste0(ceiling((x)/1024^2 * 100)/100, " MB"))
    if (x > 10^4) 
        return(paste0(ceiling((x)/1024 * 100)/100, " KB"))
    
    return(paste0(x, " Byte"))
}


# This function query the device info and storage the results into the
# global variable If initialOnly=TRUE the function will only update the
# deviceInfo in the first call
updateDeviceInfo <- function(initialOnly = FALSE) {
    if (initialOnly) {
        if (!is.null(.gpuResourcesManager$globalVars$deviceInfo)) 
            return()
    }
  
    platformNum = getPlatformNum()
    deviceInfo = c()
    id = 0
    for (i in seq_len(platformNum) - 1) {
        deviceNum = getDeviceNum(i)
        for (j in seq_len(deviceNum) - 1) {
            curDeviceInfo = getSingleDeviceInfo(i, j)
            curDeviceInfo$id = id
            deviceInfo = rbind(deviceInfo, curDeviceInfo)
            id = id + 1
        }
    }
    
    if (length(deviceInfo) == 0) {
      stop("No device has been found, please make sure the computer has a graphic card or the driver has been properly installed.\n",
              "Hint:", 
              "\nFor CPU, you can install the intel's / ATI's graphic driver for the intel's / AMD's CPU respectively.", 
              "\nFor GPU, you need to download the graphic driver from your vendor's website.")
    }
    deviceInfo$haslocalMemory = deviceInfo$haslocalMemory == 1
    deviceInfo$id = as.integer(deviceInfo$id + 1)
    deviceInfo$platform = as.integer(deviceInfo$platform + 1)
    deviceInfo$device = as.integer(deviceInfo$device + 1)
    .gpuResourcesManager$globalVars$deviceInfo = deviceInfo
}

selectDevice = function(devices) {
    .gpuResourcesManager$releaseAll()
    updateDeviceInfo(initialOnly = TRUE)
    deviceInfo = .gpuResourcesManager$globalVars$deviceInfo
    
    if (range(devices)[1] <= 0 || range(devices)[2] > nrow(deviceInfo)) {
        stop("Invalid device id!")
    }
    
    curDeviceInfo = deviceInfo[devices, , drop = FALSE]
    
    clear(.gpuResourcesManager$internalVars$addressSizeList)
    clear(.gpuResourcesManager$internalVars$addressList)
    clear(.gpuResourcesManager$internalVars$memoryUsage)
    clear(.gpuResourcesManager$internalVars$totalMemory)
    clear(.gpuResourcesManager$globalVars$curDevice)
    
    for (i in seq_len(length(devices))) {
        key = as.character(devices[i])
        .gpuResourcesManager$internalVars$addressSizeList[[key]] = hash()
        .gpuResourcesManager$internalVars$addressList[[key]] = hash()
        .gpuResourcesManager$globalVars$curDevice[[key]] = as.integer(curDeviceInfo[i, 
            c("platform", "device"), drop = FALSE] - 1)
        .gpuResourcesManager$internalVars$totalMemory[[key]] = as.double(curDeviceInfo[i, 
            "globalMemory"])
        .gpuResourcesManager$internalVars$memoryUsage[[key]] = 0
    }
}

getFirstSelectedDevice <- function() {
    deviceList = sort(as.integer(keys(.gpuResourcesManager$globalVars$curDevice)))
    as.integer(deviceList[1])
}
# Get the platform and device id If checkInitial=TRUE, the function
# will check if the device has been selected If checkInitial=FALSE, the
# function will just return the device information without check
getSelectedDevice <- function(device, checkInital = TRUE) {
    if (checkInital) {
        devKey = as.character(device)
        if (!isDeviceSelected(devKey)) 
            stop("The device has not been initialized! Please call setDevice() to initialize the device before use it.")
        
        return(as.integer(.gpuResourcesManager$globalVars$curDevice[[devKey]]))
    } else {
        updateDeviceInfo(initialOnly = TRUE)
        curDevice = as.integer(.gpuResourcesManager$globalVars$deviceInfo[device, 
            c("platform", "device")] - 1)
        if (is.null(curDevice)) 
            stop("the device does not exist!")
        return(curDevice)
    }
}
isDeviceSelected <- function(device) {
    devKey = as.character(device)
    has.key(devKey, .gpuResourcesManager$globalVars$curDevice)
}

getTrueAd <- function(address) {
    .Call(C_getTrueAd, address)
}
