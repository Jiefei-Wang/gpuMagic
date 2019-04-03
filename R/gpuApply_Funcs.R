
saveGPUcode <- function(GPUcode) {
    GPUcode_hash = GPUcode
    GPUcode_hash$parms = NULL
    for (i in seq_along(GPUcode_hash$varInfo)) {
        if (is(GPUcode_hash$varInfo[[i]],"hash")) 
            GPUcode_hash$varInfo[[i]] = copy(GPUcode_hash$varInfo[[i]])
    }
    GPUcode_hash$parmsName = names(GPUcode$parms)
    GPUcode_hash
}
loadGPUcode <- function(key, parms) {
    GPUcode = gpuApplyFuncList[[key]]
    GPUcode[["parms"]] = parms
    names(GPUcode$parms) = GPUcode$parmsName
    return(GPUcode)
}


createSapplySignature <- function(parms, FUN, .macroParms, .device, .options) {
    sig = c()
    parmsName = names(parms)
    
    res=processDimTbl(parms,.macroParms)
    matchRule=res$rule
    # skip the first parameter(the parameter that will be looped on)
    for (i in seq_len(length(parms) - 1) + 1) {
        # Type of the parameters
        varSig = ""
        # Precision type of the parameter when it is a gpuMatrix class
        if (is(parms[[i]],"gpuMatrix")) {
            varSig = paste0(varSig, .type(parms[[i]]))
        }
        # When it is a macro, add the dim and data
        sig = c(sig, varSig)
    }
    sig=c(sig,matchRule)
    # Default variable type
    sig = c(sig, paste(GPUVar$default_float, GPUVar$default_int, GPUVar$default_index_type, 
        sep = ","))
    # gpuSapply options
    sig = c(sig,digest(FUN), digest(.options$sapplyOptimization))
    sig
}


getVarSizeInfo_C_level <- function(sizeMatrix) {
    matrixOffset = c()
    size1 = sizeMatrix$size1
    size2 = sizeMatrix$size2
    size1[is.na(size1)]=0
    size2[is.na(size2)]=0
    curoffSet = 0
    for (i in seq_len(nrow(sizeMatrix))) {
        curInfo = sizeMatrix[i, ]
        matrixOffset[i] = curoffSet
        curoffSet = curoffSet + curInfo$sizeInByte
    }
    
    matrixNum = nrow(sizeMatrix)
    dim = c(size1, size2)
    if (curoffSet == 0) 
        curoffSet = 1
    # if(is.null(size1)) size1=0 if(is.null(size2)) size2=0
    res = list(matrixOffset = matrixOffset, size1 = size1, size2 = size2, 
        dim = dim, totalSize = curoffSet, matrixNum = matrixNum)
    return(res)
}
fillGPUdata <- function(GPUcode1, .options, .device) {
    parms = GPUcode1$parms
    varInfo = GPUcode1$varInfo
    
    # Convert all the parameters into the gpuMatrix objects
    for (varName in names(parms)) {
        if (is(parms[[varName]], "gpuMatrix")) {
            curInfo = getVarInfo(varInfo, varName)
            if (curInfo$precisionType != .type(parms[[varName]])) {
                stop("The data type of the variable ", varName, " are not compatible with the code,\n", 
                  "expected type: ", curInfo$precisionType, ", variable type:", 
                  .type(parms[[varName]]), "\n")
            }
            if (.device(parms[[varName]]) == .device) {
                next
            } else {
                warning("You supplied a gpu memory object but it does not belong to the device that the code will be run on.")
                parms[[varName]] = as.matrix(parms[[varName]])
            }
        }
        curInfo = getVarInfo(varInfo, varName, 1)
        curType = curInfo$precisionType
        parms[[varName]] = gpuMatrix(parms[[varName]], type = curType, 
            device = .device)
    }
    
    
    kernel_args = list()
    
    # return size, gp,gs,lp offset, gp,gs,lp,ls number, gp,gs,lp,ls dim(row,col) 
    kernel_args$sizeInfo = NULL
    returnSize=0
    
    
    #The size of the matrix in gp,gs,lp,ls
    matrix_size_info=c()
    
    sizeInfo_gp = getVarSizeInfo_C_level(varInfo$matrix_gp)
    # Total size per worker
    matrix_size_info=c(matrix_size_info,sizeInfo_gp$dim)
    
    
    sizeInfo_gs = getVarSizeInfo_C_level(varInfo$matrix_gs)
    matrix_size_info=c(matrix_size_info,sizeInfo_gs$dim)
    
    sizeInfo_lp = getVarSizeInfo_C_level(varInfo$matrix_lp)
    matrix_size_info=c(matrix_size_info,sizeInfo_lp$dim)
    
    sizeInfo_ls = getVarSizeInfo_C_level(varInfo$matrix_ls)
    matrix_size_info=c(matrix_size_info,sizeInfo_ls$dim)
    
    
    
    
    if (!is.null(varInfo$returnInfo)) {
        returnInfo = varInfo$returnInfo
        if(sum(is.na(returnInfo$designSize))>0) {
          warning("Undetermined return size has been found!")
          returnInfo=returnInfo[!is.na(returnInfo$designSize),]
        }
        if (length(returnInfo$designSize) != 0) {
            if (sum(returnInfo$designSize[1] != returnInfo$designSize) > 0) 
                warning("Multiple return size has been found!")
            
          returnSize = max(max(returnInfo$designSize),1)
        }
    }
    
    
    totalWorkerNum = length(parms[[1]])
    
    global_gp_offset=sizeInfo_gp$matrixOffset*totalWorkerNum
    matrix_offset=c(global_gp_offset,sizeInfo_gs$matrixOffset,sizeInfo_ls$matrixOffset)
    kernel_args$sizeInfo = c(returnSize,totalWorkerNum,matrix_offset,matrix_size_info)
    
    if(length(kernel_args$sizeInfo)==0) kernel_args$sizeInfo=0
    # Allocate the gpu memory
    IntType = GPUVar$default_index_type
    
    device_argument = list()
    device_argument$gp_data = gpuEmptMatrix(row = ceiling(sizeInfo_gp$totalSize *totalWorkerNum/4),
                                            col = 1, type = "int", device = .device)
    device_argument$gs_data = gpuEmptMatrix(row = ceiling(sizeInfo_gs$totalSize/4), 
                                            type = "int", device = .device)
    device_argument$ls_data = kernel.getSharedMem(sizeInfo_ls$totalSize, 
                                                  type = "char")
    # The return size for each thread
    if(returnSize!=0){
    device_argument$return_var = gpuEmptMatrix(returnSize, totalWorkerNum, type = GPUVar$default_float, device = .device)
    }else{
      device_argument$return_var = gpuEmptMatrix(1,1, type = GPUVar$default_float, device = .device)
    }
    device_argument$sizeInfo = gpuMatrix(kernel_args$sizeInfo, type = IntType, device = .device)
    
    
    device_argument = c(parms, device_argument)
    GPUcode1$device_argument = device_argument
    GPUcode1
}

# add the function definition
completeGPUcode <- function(GPUcode) {
    varInfo = GPUcode$varInfo
    profile = varInfo$profile
    GPUVar$functionCount = GPUVar$functionCount + 1
    kernelName = paste0(GPUVar$functionName, "_", GPUVar$functionCount)
    # Fefine function name
    code = paste0("kernel void ", kernelName, "(")
    # The function arguments
    kernel_arg_code = c()
    for (curName in varInfo$requiredVar) {
        curInfo = getVarInfo(varInfo, curName)
        curType = curInfo$precisionType
        kernel_arg_code = c(kernel_arg_code, paste0("global ", curType, 
            "* ", curName))
    }
    code = paste0(code, paste0(kernel_arg_code, collapse = ","))
    
    # The working memory space
    arg_prefix_list = c(
      "global", "global", "local",
      "global", "global")
    arg_list = c(
      GPUVar$global_private_data,GPUVar$global_shared_data,
      GPUVar$local_shared_data,GPUVar$return_variable, GPUVar$size_info)
    
    indType=GPUVar$default_index_type
    floatType=GPUVar$default_float
    arg_type_list = c("char", "char","char", floatType,indType)
    for (i in seq_along(arg_list)) {
        curCode = paste0(arg_prefix_list[i], " ", arg_type_list[i], "* ", 
            arg_list[i])
        if (i != length(arg_list)) 
            curCode = paste0(curCode)
        code = c(code, curCode)
    }
    paste0(arg_prefix_list, " ", arg_type_list, "* ", arg_list)
    code=paste0(code, collapse = ",\n")
    
    
    
    
    
    
    # add the kernel function definition
    code = paste0(
      code, 
      "){\n", 
      paste0(GPUcode$gpu_code, collapse = "\n"), 
      "\n}")
    
    code=c(
      paste0("#define default_index_type ",GPUVar$default_index_type),
      paste0("#define default_float ",GPUVar$default_float),
      paste0("#define default_int ",GPUVar$default_int),
      code
    )
    
    # Add the double vector support if appliable
    if (GPUVar$default_float == "double") 
        code = c("#pragma OPENCL EXTENSION cl_khr_fp64:enable", 
            code)
    
    code=paste0(code, collapse = "\n")
    
    
    GPUcode$gpu_code = code
    GPUcode$kernel = kernelName
    
    GPUcode
}

evaluateProfileTbl <- function(parms, table) {
  if (is.null(table) || nrow(table) == 0) 
    return(table)
  table$size1 = vapply(as.list(parse(text = table$size1)), eval, numeric(1), 
                       envir = environment())
  table$size2 = vapply(as.list(parse(text = table$size2)), eval, numeric(1), 
                       envir = environment())
  if(!is.null(table$sizeInByte))
    table$sizeInByte = vapply(as.list(parse(text = table$sizeInByte)), eval, 
                              numeric(1), envir = environment())
    table$designSize = table$size1*table$size2
  return(table)
}


completeProfileTbl <- function(GPUExp3) {
    parms = GPUExp3$parms
    parms=lapply(parms,function(curParm){
      if(!is(curParm,"gpuMatrix")&&!is(curParm,"matrix")){
        message(is(curParm,"gpuMatrix"))
        curParm=as.matrix(curParm)
      }
      curParm
    })
    varInfo = GPUExp3$varInfo
    varInfo$matrix_gs = evaluateProfileTbl(parms, varInfo$matrix_gs)
    varInfo$matrix_gp = evaluateProfileTbl(parms, varInfo$matrix_gp)
    varInfo$matrix_ls = evaluateProfileTbl(parms, varInfo$matrix_ls)
    varInfo$matrix_lp = evaluateProfileTbl(parms, varInfo$matrix_lp)
    
    
    
    varInfo$returnInfo = evaluateProfileTbl(parms, varInfo$returnInfo)
    
    
    GPUExp3$varInfo = varInfo
    GPUExp3
    
}
CheckCodeError <- function(GPUcode) {
  parms=GPUcode$parms
    errorCheckInfo = GPUcode$errorCheck
    if(is.null(errorCheckInfo)) return()
    for (i in seq_len(nrow(errorCheckInfo))) {
        info = errorCheckInfo[i, ]
        if (info$check == "") 
            next
        error = eval(parse(text = info$check))
        if (is.na(error) || is.null(error)) 
            next
        if (error) {
            if (info$level == "warning") {
                warning(info$msg, ": \n", info$code)
            } else {
                stop(info$msg, ": \n", info$code)
            }
        }
    }
}

matchParms <- function(X, parms, FUN) {
    
    argNames = names(funcToExp(FUN)$args)
    loopVar_ind = which(argNames == "X")
    if (length(loopVar_ind) == 0) 
        loopVar = argNames[1] else loopVar = "X"
    parms = c(list(loopVar = X), parms)
    names(parms)[1] = loopVar
    unmatchedName = setdiff(argNames, names(parms))
    parName = names(parms)
    for (i in seq_along(parName)) {
        if (parName[i] == "") {
            if (length(unmatchedName) > 0) {
                parName[i] = unmatchedName[1]
                unmatchedName = unmatchedName[-1]
            } else stop("The function arguments does not match")
        }
    }
    if (length(unmatchedName) > 0) {
        stop("The function arguments does not match")
    }
    names(parms) = parName
    parms
}

formatParms <- function(parms) {
    for (i in seq_along(parms)) {
        if (!is(parms[[i]],"gpuMatrix") && !is(parms[[i]],"matrix")) {
            parms[[i]] = as.matrix(parms[[i]])
        }
    }
    parms
}

# =========================optimization functions==============================
opt_workerNumber <- function(varInfo, code, .options) {
    targetCode = paste0("//Thread number optimization\n")
    
    if (!grepl(targetCode, code, fixed = TRUE)) {
        stop("Unable to find the location of the thread number optimization code\n", 
            "This error should never be happened\n", "Please contact the author")
    }
    loopedVar_length=GPUVar$gpu_global_size
    
    if (.options$sapplyOptimization$thread.number) {
        insertedCode = paste0("if(", GPUVar$gpu_global_id, "<", loopedVar_length, "){\n")
        insertedCode = paste0(targetCode, insertedCode)
        endCode = "\n}"
    } else {
        insertedCode = ""
        endCode = ""
    }
    code = sub(targetCode, insertedCode, code, fixed = TRUE)
    code = paste0(code, endCode)
    code
}
