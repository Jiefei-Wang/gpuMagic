#' @include hash.R
gpuApplyFuncList = hash()


#' A GPU version sapply function
#' 
#' Please refer to sapply to see the basic usage
#' 
#' This function compile the R code and run it on the openCL-compatible devices. The usage is similar to the sapply function with 
#' some addtional opencl-related arguments.
#' 
#' @param X a vector that `FUN` will loop over.
#' @param FUN The function to be applied to each elemtn of `X`
#' @param ... optional arguments to `FUN`
#' @param .macroParms 
#' The function argument that will be treated as macro in the code. 
#' If an argument is treated as macro, its value cannot be changed by the code
#' @param .device The device that the function will be excuted. Running the code on Multiple devices is supported but is still under development
#' @param loading The loading of each device, only useful when having multiple devices.
#' @param .options The package and openCL compilation options, please call `gpuSapply.getOption()` to get all the available options
#' 
#' @examples
#' #matrix multiplication function
#' matMul<-function(ind,A,B){
#' C=A%*%B[,ind]
#' return(C)
#' }
#' 
#' n=100
#' m=200
#' k=100
#' #Create the data
#' A=matrix(runif(n*m),n,m)
#' B=matrix(runif(k*m),m,k)
#' #Perform matrix multiplication
#' #GPU
#' res_gpu=gpuSapply(1:k,matMul,A,B)
#' #CPU
#' res_cpu=sapply(1:k,matMul,A,B)
#' 
#' #error
#' range(res_gpu-res_cpu)
#' @export
#' @return A vector or a matrix
gpuSapply <- function(X, FUN, ..., .macroParms = NULL, .device = "auto", 
    loading = "auto", .options = gpuSapply.getOption()) {
    if (.device == "auto") {
        .device = as.integer(keys(.gpuResourcesManager$globalVars$curDevice))
    } else {
        .device = as.integer(.device)
    }
    
    deviceNum = length(.device)
    # If the number of device is 1, just call the single device function
    if (deviceNum == 1) {
        res = gpuSapply_singleDev(X, FUN, ..., .macroParms = .macroParms, 
            .device = .device, .options = .options)
    } else {
        res = gpuSapply_multiDev(X, FUN, ..., .macroParms = .macroParms, 
            .device = .device, loading = "auto", .options = .options)
    }
    return(res)
}

gpuSapply_singleDev <- function(X, FUN, ..., .macroParms = NULL, .device, 
    .options = gpuSapply.getOption(), .block = TRUE) {
    # Some interesting setup
    start_time <- Sys.time()
    verbose = .options$verbose
    optimization = .options$sapplyOptimization
    msg = .options$sapplyMsg
    option = .options$sapplyOption
    
    if (verbose || sum(as.matrix(msg)) != 0) 
        message("======gpuSapply compilation======")
    # Check and match the parameter names
    parms = list(...)
    parms = matchParms(X, parms, FUN)
    # Convert all the parameters to matrix type
    parms = formatParms(parms)
    # Create the signature for the functions
    sig = createSapplySignature(parms, FUN, .macroParms, .device, .options)
    sig_hash = digest(sig)
    # Check if the compiled code exist, if not, compile the function
    if (has.key(sig_hash, gpuApplyFuncList) && option$debugCode == "" && 
        !option$compileEveryTime) {
        if (verbose || msg$R.code.compiler.msg) {
            message("The R function has been compiled.")
        }
        GPUcode1 = loadGPUcode(sig_hash, parms)
    } else {
        if (verbose || msg$R.code.compiler.msg) {
            message("The R function has not been compiled.")
        }
        GPUcode1 = .compileGPUCode(FUN, parms, .macroParms = .macroParms, 
            .options = .options)
        
        # Store the GPU object
        gpuApplyFuncList[[sig_hash]] = saveGPUcode(GPUcode1)
        
        # insert debug code, when debug code is not empty, the function will be
        # compiled every time
        if (option$debugCode != "") {
            GPUcode1$gpu_code = option$debugCode
            GPUcode1$kernel = gsub(".+kernel void ([^(]+)\\(.+", "\\1", 
                option$debugCode)
        }
    }
    
    
    
    
    
    # Complete the profile table and fill the GPU data
    GPUcode1 = completeProfileTbl(GPUcode1)
    CheckCodeError(GPUcode1, parms)
    GPUcode2 = fillGPUdata(GPUcode1, .options = .options, .device = .device)
    
    .options$kernelOption$localThreadNumMacro = TRUE
    if (optimization$thread.number == TRUE) {
        if (.options$kernelOption$localThreadNum == "auto") 
            kernelNum = as.numeric(gpuMagic.getOptions("default.thread.num")) else kernelNum = .options$kernelOption$localThreadNum
        
        .globalThreadNum = ceiling(length(X)/kernelNum) * kernelNum
        .options$kernelOption$localThreadNum = kernelNum
    } else {
        .globalThreadNum = length(X)
    }
    
    if (verbose || msg$timing.R.code.compilation) {
        end_time <- Sys.time()
        compileTime = round(as.numeric(end_time - start_time), digits = 3)
        message("Total R code compilation time: ", compileTime, " secs")
    }
    
    # .options$signature=c(.options$signature,sig_hash)
    .kernel(kernel = GPUcode2$kernel, src = GPUcode2$gpu_code, parms = GPUcode2$device_argument, 
        .device = .device, .globalThreadNum = .globalThreadNum, .options = .options)
    res = GPUcode2$device_argument$return_var
    
    
    if (.block) {
        res = download(res)
        res = as.matrix(res)
    }
    return(res)
}

gpuSapply_multiDev <- function(X, FUN, ..., .macroParms = NULL, .device, 
    loading = "auto", .options = gpuSapply.getOption(), .block = TRUE) {
    deviceNum = length(.device)
    jobsNum = length(X)
    # If the number of device is larger than 1, parallel the process Find
    # the loading for each device
    if (loading == "auto" || length(loading) != deviceNum) {
        loading = rep(1, deviceNum)
    }
    loading = loading/sum(loading)
    
    # Create job blocks
    jobs = c()
    startInd = 0
    for (i in seq_len(deviceNum)) {
        endInd = min(startInd + ceiling(jobsNum * loading[i]), jobsNum)
        jobs = rbind(jobs, c(startInd, endInd))
        startInd = min(endInd, jobsNum)
    }
    
    # Parallel the jobs
    parallelSet = list()
    for (i in seq_len(deviceNum)) {
        start = jobs[i, 1]
        end = jobs[i, 2]
        if (end - start == 0) {
            parallelSet[[i]] = NULL
        } else {
            parallelSet[[i]] = gpuSapply_singleDev(X[(start + 1):end], 
                FUN, ..., .macroParms = .macroParms, .device = .device[i], 
                .options = .options, .block = FALSE)
        }
    }
    # Get the result back
    unfinishedDevice = seq_len(deviceNum)
    unfinishedDeviceNum = deviceNum
    while (unfinishedDeviceNum != 0) {
        for (i in seq_len(deviceNum)) {
            ind = unfinishedDevice[i]
            if (ind == 0) 
                next
            curDev = .device(parallelSet[[ind]])
            if (getJobStatus(curDev) == "complete") {
                unfinishedDevice[i] = 0
                unfinishedDeviceNum = unfinishedDeviceNum - 1
                parallelSet[[i]] = as.vector(download(parallelSet[[i]]))
            }
        }
    }
    res = unlist(parallelSet)
    dim = c(0, 0)
    dim[1] = length(res)/jobsNum
    dim[2] = jobsNum
    .Call(C_asMatrix, res, as.integer(dim))
    return(res)
}






#' Get the package compilation options
#' 
#' Get the package compilation options, the openCl compilation options(`kernel.getOption()`) are also included.
#' 
#' @details 
#' There are a few options that is allowed to be changed, they are:
#' `sapplyOption.debugCode`: Replace the compiled GPU code with your customized code, this option is
#' useful when you want to debug the compiled code, or when you want to customize the compiled code.
#' 
#' `sapplyOption.compileEveryTime`: Specify whether you want the compiler to compile the R code everytime.
#' 
#' @examples 
#' opt=gpuSapply.getOption()
#' @return An options class
#' @export
gpuSapply.getOption <- function() {
    curOp = kernel.getOption()
    curOp$kernelOption$autoType = FALSE
    curOp$sapplyMsg = data.frame(R.code.compiler.msg = FALSE, timing.R.code.compilation = FALSE)
    
    curOp$sapplyOptimization = data.frame(thread.number = TRUE, matrix.dim = TRUE)
    
    curOp$sapplyOption = data.frame(debugCode = "", compileEveryTime = FALSE, 
        stringsAsFactors = FALSE)
    
    curOp = structure(curOp, class = "options")
    return(curOp)
}

#' Compile the R function without excute it in the device.
#' 
#' @inheritParams gpuSapply
#' @examples
#' #matrix add function
#' matAdd<-function(ind,A,B){
#' C=A[,ind]+B[,ind]
#' return(C)
#' }
#' 
#' n=100
#' m=200
#' #Create the data
#' A=matrix(runif(n*m),n,m)
#' B=matrix(runif(n*m),n,m)
#' #Compile the R code
#' res=compileGPUCode(1:m,matAdd,A,B)
#' #print GPU code
#' cat(res$gpu_code)
#' @return A list of compilation information
#' 
#' @export
compileGPUCode <- function(X, FUN, ..., .macroParms = NULL, .options = gpuSapply.getOption()) {
    parms = list(...)
    parms = matchParms(X, parms, FUN)
    parms = formatParms(parms)
    GPUcode1 = .compileGPUCode(FUN, parms, .macroParms = .macroParms, .options = .options)
}

.compileGPUCode <- function(FUN, parms, .macroParms = NULL, .options) {
    codeMetaInfo = list()
    codeMetaInfo$Exp = funcToExp(FUN)$code
    codeMetaInfo$parms = parms
    codeMetaInfo$macroParms = .macroParms
    
    
    codeMetaInfo0 = codePreprocessing(codeMetaInfo)
    codeMetaInfo1 = RParser1(codeMetaInfo0)
    codeMetaInfo2 = RParser2(codeMetaInfo1)
    profileMeta1 = RProfile1(codeMetaInfo2)
    profileMeta2 = RProfile2(profileMeta1)
    # profileMeta3=RRecompiler(profileMeta2)
    GPUExp1 = RCcompilerLevel1(profileMeta2)
    GPUExp2 = RCcompilerLevel2(GPUExp1)
    
    
    GPUcode1 = completeGPUcode(GPUExp2)
    
    # optimization
    GPUcode1$gpu_code = opt_workerNumber(GPUcode1$varInfo, GPUcode1$gpu_code, 
        .options)
    GPUcode1$gpu_code = opt_matrixDim(GPUcode1$varInfo, GPUcode1$gpu_code, 
        .options)
    
    
    GPUcode1
}

