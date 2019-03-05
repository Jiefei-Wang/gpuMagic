checkTypeSupport <- function(type) {
  if (!(type %in% gpuMagic.getAvailableType())) 
    stop("The variable type ", type, " is not supported")
}
getTypeNum <- function(type) {
  switch(type, bool = 1L, char = 1L, half = 2L, float = 3L, double = 4L, 
         int = 5L, long = 6L, uint = 7L, ulong = 8L, stop("invalid type: ", 
                                                          type))
}

getFloatingPointType<-function(type){
  c("half","float","double")
}
getIntegerType<-function(type){
  c("bool","char","uint","int","ulong","long")
}
getTypeCXXStr <- function(type) {
  switch(type, bool = "bool", char = "char", half = "half", float = "float", 
         double = "double", int = "int", long = "long", uint = "uint", ulong = "ulong", 
         stop("invalid type: ", type))
}

# in byte
getTypeSize <- function(type) {
  switch(type, bool = 1L, char = 1L, half = 2L, float = 4L, double = 8L, 
         int = 4L, long = 8L, uint = 4L, ulong = 8L, stop("invalid type: ", 
                                                          type))
}
getDataType <- function(data) {
  if (typeof(data) == "double" || typeof(data) == "numeric") 
    return(GPUVar$default_float)
  if (typeof(data) == "integer") 
    return(as.character(gpuMagic.getOptions("default.int")))
  stop("The given type is not defined")
}
convertDataType <- function(data, type) {
  checkTypeSupport(type)
  switch(type, bool = as.raw(data), char = as.raw(data), int = as.integer(data), 
         as.double(data))
}
#' Print the available options in a pretty format
#' 
#' @param x an options object.
#' @param ... just for making the package checking happy.
#' @examples 
#' opt=gpuMagic.getOptions()
#' print(opt)
#' @return No return value, the result will be printed in the console
#' @rdname printFunctions
#' @method print options
#' @export
print.options <- function(x, ...) {
    x = unlist(x)
    name = StrAlign(names(x), sep = "\\l")
    value = StrAlign(as.character(x), sep = "\\l")
    final = paste0(paste(name, value, sep = ": "), collapse = "\n")
    cat(final,"\n")
}

#' @rdname printFunctions
#' @method print plainText
#' @export
print.plainText<-function(x,...){
  cat(x)
}

# ===========================Obtain device infomation==============
#' Query and select the devices
#' 
#' This is a set of functions to query the device information and select which device should be used in the computation
#' 
#' @details 'getDeviceList()': The function is used to obtain all the opencl-enable devices
#' @examples 
#' #Get the available devices
#' getDeviceList()
#' 
#' @return 'getDeviceList()': No return value, the result will be printed in the console
#' @rdname DeviceInfo
#' @export
getDeviceList = function() {
    updateDeviceInfo()
    deviceInfo = .gpuResourcesManager$globalVars$deviceInfo[, c("id", "platform", 
        "device", "deviceName", "globalMemory")]
    deviceInfo$globalMemory = vapply(deviceInfo$globalMemory, format_memory_size_output,character(1))
    print(deviceInfo, row.names = FALSE, right = FALSE)
    invisible()
}

#' @details 'getDeviceInfo()': Get the ith device information, call 'getDeviceList()' first to figure out the index before use this function
#' @param i A 1-based device index, it should be an integer
#' @examples 
#' #Get the information of the first device
#' getDeviceInfo(1)
#' @return 'getDeviceInfo()': A list with the device information
#' @rdname DeviceInfo
#' @export
getDeviceInfo = function(i) {
    updateDeviceInfo()
    deviceInfo = .gpuResourcesManager$globalVars$deviceInfo
    if (i > nrow(deviceInfo) || i <= 0) {
        stop("Invalid device id!")
    }
    deviceInfo = deviceInfo[i, , drop = FALSE]
    deviceInfo$globalMemory = format_memory_size_output(deviceInfo$globalMemory)
    deviceInfo$localMemory = format_memory_size_output(deviceInfo$localMemory)
    deviceInfo = structure(deviceInfo, class = "options")
    deviceInfo
}

#' @details 'getCurDevice()': Get the information of the current devices
#' @examples 
#' #Get the information of current used devices
#' getCurDevice()
#' @return 'getCurDevice()': No return value, the result will be printed in the console
#' @rdname DeviceInfo
#' @export
getCurDevice = function() {
    curInd = as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
    for (i in curInd) {
        print(getDeviceInfo(i))
        cat("\n\n")
    }
    invisible()
}
#' @details 'setDevice()': Set which device will be used in the opencl, 
#' call 'getDeviceList()' first to figure out the index before use this function
#' @inheritParams getDeviceInfo
#' @examples
#' #Use the first device
#' setDevice(1)
#' #Use two devices
#' #setDevice(c(1,2))
#' @return 'setDevice()': No return value
#' @rdname DeviceInfo
#' @export
setDevice = function(i) {
    selectDevice(sort(unique(as.integer(i))))
    invisible()
}
#' @details 'getDeviceIndex()': Get the index of the current devices 
#' @examples
#' #Get the index of the current devices
#' getDeviceIndex()
#' @return 'getDeviceIndex()': An integer representing the device index
#' @rdname DeviceInfo
#' @export
getDeviceIndex = function() {
    as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
}



#' @details 'getJobStatus()': Query the current job status in a device
#' 
#' @inheritParams getDeviceInfo
#' @examples 
#' #Get the job status in the first device
#' getJobStatus(1)
#' @return 'getJobStatus()': A character representing the device status
#' @rdname DeviceInfo
#' @export
getJobStatus = function(i) {
    device = getSelectedDevice(i)
    status = .Call(C_getDeviceStatus, device[1], device[2])
    switch(as.character(status), `3` = "queued", `2` = "submitted", `1` = "running", 
        `0` = "complete", paste0("Unknown status:", status))
}





# ===========================Package functions==============

gpuMagic.options = new.env()
gpuMagic.options$default.thread.num = 64
gpuMagic.options$hoist.optimization=FALSE
gpuMagic.options$supportedType <- c("bool", "char", "half", "float", "double", 
    "int", "long", "uint", "ulong")



#' Get the openCL options
#' 
#' The functions get the computing precision when compile the GPU code and the number of workers in a computing group.
#' 
#' The fields `default.float`, `default.int` and `default.index.type` are used to control the computing precision. 
#' When transferring data from R to GPU, if the data in R has a numeric or double storage mode,
#'`default.float` will be used to convert data type.
#' Similarly, If the data has an Integer storage model. `default.int` will be used.
#' 
#' `default.index.type` controls the variable type for the for loop index, variable dimension etc.  
#' 
#' `default.thread.num` is used to control the number of workers in a group in openCL. It is not expected to be changed unless you know what you are doing.
#' 
#' @param opt The options that the function will return. It can be either 'all' or a vector of the option names.
#' 
#' @examples 
#' #Get all the available options
#' opt=gpuMagic.getOptions()
#' opt
#' @return A list of the options
#' @export
gpuMagic.getOptions = function(opt = "all") {
  allOpt = data.frame(
    default.float = GPUVar$default_float, 
    default.int = GPUVar$default_int, 
    default.index.type = GPUVar$default_index_type, 
    default.thread.num = gpuMagic.options$default.thread.num,
    hoist.optimization=gpuMagic.options$hoist.optimization,
    stringsAsFactors = FALSE)
  if (opt == "all") 
    curOpt = allOpt 
  else 
    curOpt = allOpt[, opt, drop = FALSE]
  curOpt = structure(curOpt, class = "options")
  curOpt
}



#'  Set the openCL options
#' 
#' The functions set the computing precision when compile the GPU code and the number of workers in a computing group.
#' 
#' @param ... 
#' There are two possible ways to set the options. You can either provide
#' 
#' 1. A named argument which name is the same as the name of the options.
#' 
#' 2. An R object obtaining from `gpuMagic.getOptions()`
#' 
#' to change the options.
#' 
#' @seealso [gpuMagic.getOptions()] for the name of the options.
#' @examples
#' #Get all the available options
#' opt=gpuMagic.getOptions()
#' #change the default float type
#' opt$default.float='float'
#' #set the options
#' gpuMagic.setOptions(opt)
#' 
#' #set the options(Alternative way)
#' gpuMagic.setOptions(default.float='float')
#' @return No return value
#' @export
gpuMagic.setOptions = function(...) {
    parms = list(...)
    if (length(parms) == 1 && is(parms[[1]],"options")) {
        parms = parms[[1]]
    }
    optNames = names(parms)
    for (i in optNames) {
        value = parms[[i]]
        switch(i, 
               default.float = {
                 checkTypeSupport(value)
                 GPUVar$default_float = value
               }, 
               default.int = {
                 checkTypeSupport(value)
                 GPUVar$default_int = value
               }, 
               default.index.type = {
                 checkTypeSupport(value)
                 GPUVar$default_index_type = value
               }
        )
        if(i %in% names(gpuMagic.options)){
          curValue=gpuMagic.options[[i]]
          if(xor(is.numeric(value),is.numeric(curValue))||
             typeof(value)!=typeof(curValue)){
            stop("Invalid option value: ",i)
          }
          gpuMagic.options[[i]]=value
        }
        
    }
}
#' Get all the available openCL variable type
#' 
#' @return A vector of all the available data type.
#' @examples 
#' gpuMagic.getAvailableType()
#' @export
gpuMagic.getAvailableType = function() {
    gpuMagic.options$supportedType
}

#' Get the device memory usage
#' 
#' The function will print the memory usage on the console
#' @examples
#' gpuMagic.getMemUsage()
#' @return No return value, the result will be printed in the console.
#' @export
gpuMagic.getMemUsage = function() {
    .gpuResourcesManager$getGPUusage()
}
