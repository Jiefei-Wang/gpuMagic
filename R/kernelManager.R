#' Excute the openCL function
#' 
#' The function serves as a bridge between R and openCL, it sends the openCL code and 
#' R matrix object to the device and excute it on the device. The function has an auto type 
#' ability which can make the openCL code independent with the type of its function argument,
#' see detail and examples for the usage.
#' 
#' @details 
#' The function `.kernel()` is the low level API to communicate with openCL device.
#' It provides a way to run the customized code on the device, the source code should be
#' openCL code and the kernel is the kernel function that you want to run on the device.
#' 
#' You can specify with device the code should be run on by specifying the `.device` argument.
#' By default, if you do not specify any device, the first device in the device list will be used
#' 
#' The argument `.globalThreadNum` specifys the number of thread that will be used to excute the kernel.
#' The concept is the same as `global_work_size`` in openCL function
#' 
#' There are multiple options that you can change in the kernel function. 
#' You can call the function `kernel.getOption()` to obtain the default setting. 
#' The most distinguishable feature in this package is probably the auto type function, which 
#' can set the type of the kernel arguments as an macro in the openCL code. This feature allow the 
#' user to create a type-free code. If the `kernelOption$autoType` in `.options` is true(Default),
#' four macros will be defined, they are(X is the position of the function arguments):
#' 
#' autoX: The variable type
#' 
#' gAutoX: Short for global autoX
#' 
#' lAutoX: short for local autoX
#' 
#' autoX_v4: Define a vector of length 4 with the same variable type as the X th function argument
#' 
#' Please refer to the example for the usage
#' 
#' 
#' @param src the source code, it can be either a file directory or the code
#' @param kernel the kernel function that will be called on the device
#' @param parms a list containning the function arguments. 
#' The number of elements in the list has to match the number of function arguments.
#' @param .device the device that will excute the function. 
#' If not specified, all the selected devices will be used.
#' @param .globalThreadNum the number of thread that will be created to excute the kernel. 
#' If not specified, the length of the first argument will be used as the thread number
#' @param .options the kernel options
#' @examples 
#' #The GPU code
#' code='
#' kernel void matAdd(gAuto1* A,gAuto2* B,gAuto3* C,gAuto4* size){
#' uint col_id=get_global_id(0);
#' uint rowNum=*size;
#' for(uint i=0;i<rowNum;i++){
#' C[i+col_id*rowNum]=A[i+col_id*rowNum]+B[i+col_id*rowNum];
#' }
#' }
#' '
#' #Create data in R
#' m=100
#' n=200
#' A=matrix(runif(m*n),m,n)
#' B=matrix(runif(m*n),m,n)
#' #Send the data to GPU
#' A_dev=gpuMatrix(A,type='double')
#' B_dev=gpuMatrix(B,type='double')
#' #Create an empty data in GPU
#' C_dev=gpuEmptMatrix(row=m,col=n,type='double')
#' 
#' #Get the default options
#' options=kernel.getOption()
#' #Run the GPU function
#' .kernel(src = code,kernel='matAdd',parms=list(A_dev,B_dev,C_dev,m),.options = options)
#' 
#' #Retrieve the data
#' C_dev=download(C_dev)
#' C=as.matrix(C_dev)
#' #Check the error
#' range(C-A-B)
#' @return A vector or a matrix
#' @export
.kernel <- function(src = "", kernel, parms, .device = "auto", .globalThreadNum = "length(FirstArg)", 
    .options = kernel.getOption()) {
    verbose = .options$verbose
    kernelMsg = .options$kernelMsg
    kernelOption = .options$kernelOption
    
    if (verbose || sum(as.matrix(kernelMsg[, -grep("warning", colnames(kernelMsg))])) != 
        0) 
        message("======kelnel compilation=========")
    # Read the opencl code
    codePack = readCode(src)
    src = codePack$src
    srcSig = codePack$srcSig
    
    
    if (kernelOption$localThreadNumMacro) {
        if (kernelOption$localThreadNum != "auto") {
            threadMacro = paste0("#define cl_local_thread_num ", kernelOption$localThreadNum)
            srcSig = paste0(srcSig, kernelOption$localThreadNum)
            src = paste0(threadMacro, "\n", src)
        } else {
            warning("I cannot define the local thread number macro if the local thread number is undetermined.")
        }
        
    }
    
    
    # Find the device and platform id
    if (length(.device) > 1) 
        stop("Multiple devices are not supported!")
    if (.device == "auto") {
        deviceId = getFirstSelectedDevice()
    } else {
        deviceId = .device
    }
    device = getSelectedDevice(deviceId)
    
    
    dataType = c()
    for (i in seq_len(length(parms))) {
        if (class(parms[[i]]) == "list") {
            dataType[i] = parms[[i]]$type
            next
        }
        if (class(parms[[i]]) != "gpuMatrix") {
            parms[[i]] = gpuMatrix(parms[[i]], device = deviceId)
        }
        dataType[i] = .type(parms[[i]])
        if (.device(parms[[i]]) != deviceId) 
            stop("The data is not in the same device!")
    }
    
    
    
    # Create the signature for the kernel function
    sig = paste0(srcSig, kernelOption$flag, kernelOption$signature)
    
    
    # Create the data type macros Add the signature if needed
    if (kernelOption$autoType && length(dataType) != 0) {
        gAUTO = paste0("#define gAuto", seq_along(dataType), " global ", 
            dataType, "\n", collapse = "")
        lAUTO = paste0("#define lAuto", seq_along(dataType), " local ", 
            dataType, "\n", collapse = "")
        pAUTO = paste0("#define auto", seq_along(dataType), " ", dataType, 
            "\n", collapse = "")
        
        AUTOVector = paste0("#define auto", seq_along(dataType), "_v4 ", 
            dataType, "4", "\n", collapse = "")
        
        src = paste0(gAUTO, lAUTO, pAUTO, AUTOVector, src)
        sig = c(sig, paste0(dataType, collapse = ""))
    }
    
    sig_hash = digest(sig)
    # Create the kernel if it does not exist
    if (!hasKernel(device, sig_hash, kernel)) {
        if (verbose || kernelMsg$compilation.msg) 
            message("OpenCL compiler message: The kernel does not exist and will be created")
        .Call(C_createKernel, device[1], device[2], sig_hash, kernelOption$flag, 
            src, kernel)
    }
    # Compute the usage of the shared memory and global memory upload the
    # parameters
    global_memory = 0
    share_memory = 0
    for (i in seq_len(length(parms))) {
        if (class(parms[[i]]) == "list") {
            share_memory = share_memory + parms[[i]]$size
            
            .Call(C_setSharedParameter, device[1], device[2], sig_hash, 
                kernel, as.integer(parms[[i]]$size), as.integer(i - 1))
        } else {
            global_memory = global_memory + getSize(parms[[i]])
            # message(getSize(parms[[i]]))
            .Call(C_setParameter, device[1], device[2], sig_hash, kernel, 
                .getAddress(parms[[i]]), as.integer(i - 1))
        }
    }
    if (verbose || kernelMsg$memory.usage.msg) {
        message("OpenCL memory usage report:")
        message("Global memory: ", format_memory_size_output(global_memory))
        message("Shared memory: ", format_memory_size_output(share_memory))
    }
    
    if (.globalThreadNum == "length(FirstArg)") {
        .globalThreadNum = length(parms[[1]])
    }
    
    if (kernelOption$localThreadNum == "auto") {
        localThreadNum = 0
        localThreadNum_output = getGroupSize(device, sig_hash, kernel)
    } else {
        localThreadNum = kernelOption$localThreadNum
        localThreadNum_output = kernelOption$localThreadNum
    }
    
    
    if (verbose || kernelMsg$thread.num.msg) {
        message("OpenCL thread Number report:")
        message(paste0("Total thread number: ", .globalThreadNum))
        message(paste0("block number: ", .globalThreadNum/localThreadNum_output))
        message(paste0("Thread number per block: ", localThreadNum_output))
    }
    
    .Call(C_launchKernel, device[1], device[2], sig_hash, kernel, as.integer(.globalThreadNum), 
        as.integer(localThreadNum))
    
    invisible()
}

#' Get the openCL compilation options
#' 
#' @details 
#' #' `verbose` turn the verbose mode on and off.
#' 
#' `kernelOption$localThreadNum` controls the local thread number in each group, 
#' the local thread number should be a divisor of the argument `.globalThreadNum`.
#' If it is set to `auto`, the suggested number of local thread number will be
#' obtained from openCL API and reduced to a divisor of `.globalThreadNum`.
#' 
#' `kernelOption$localThreadNumMacro` specifys whether the local thread number 
#' should be inserted into the code as an macro. If it is TRUE, the macro 
#' `cl_local_thread_num` will be defined. It is useful when you want to dynamically
#' allocate the memory \(Mostly local memory\) according to the local thread number
#' 
#' `kernelOption$signature` This is for internal usage only, please do not change it
#' 
#' `kernelOption$flag` The openCL compiler flag.
#' 
#' `kernelOption$autoType` Determine whether the type of kernel arguments should be 
#' defined as an macro, see the `.kernel` document for detail
#' @examples 
#' opt=kernel.getOption()
#' opt
#' @return A list of available options
#' @export
kernel.getOption <- function() {
    curOp = list()
    curOp$verbose = FALSE
    
    curOp$kernelMsg = data.frame(compilation.msg = FALSE, memory.usage.msg = FALSE, 
        thread.num.msg = FALSE, insufficient.thread.num.warning = TRUE)
    
    curOp$kernelOption = data.frame(localThreadNum = "auto", localThreadNumMacro = FALSE, 
        signature = "", flag = "", autoType = TRUE, stringsAsFactors = FALSE)
    curOp = structure(curOp, class = "options")
    curOp
}

kernel.getSharedMem <- function(length, type) {
    checkTypeSupport(type)
    return(list(length = length, size = length * getTypeSize(type), type = type))
}

readCode <- function(src) {
    # if the src can be treated as a file name, then read the file,
    # otherwise check if it is code.
    if (nchar(src) < 128 && file.exists(src)) {
        fileName = src
        ## Read source file
        src = readChar(fileName, file.info(fileName)$size)
        sig = as.character(file.mtime(fileName))
        # Add a space to make it more stable
        src = paste0(" ", src)
    } else {
        if (nchar(src) < 40) {
            if (length(grep("\\(.*\\)", src)) == 0) {
                warning("It looks like the src variable is a file address, but I cannot find it.")
            }
        }
        sig = digest(src)
    }
    return(list(src = src, srcSig = sig))
}


# Check if the kernel is already exist
hasKernel <- function(device, sig, kernel) {
    res = .Call(C_hasKernel, device[1], device[2], sig, kernel)
    res
}

getGroupSize <- function(device, sig, kernel) {
    res = .Call(C_getPreferredGroupSize, device[1], device[2], sig, kernel)
    res
}
