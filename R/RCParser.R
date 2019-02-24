#' @include pkgFunc.R

RCcompilerLevel1 <- function(optMeta1) {
    GPUExp1 = optMeta1
    if (DEBUG) {
        GPUExp1$varInfo = copyVarInfoTbl(optMeta1$varInfo)
    }
    parsedExp = GPUExp1$Exp
    varInfo = GPUExp1$varInfo
    
    
    gpu_matrix_size_info=GPUVar$matrix_size_info
  
    
    
    # Preserved variables Global worker private data
    gpu_gp_data = GPUVar$global_private_data
    # Per worker length
    gpu_gp_totalSize = GPUVar$global_private_totalSize
    
    # matrix index offset
    gpu_gp_offset = GPUVar$global_private_offset
    
    # worker shared data, located in global memory
    gpu_gs_data = GPUVar$global_shared_data
    gpu_gs_offset = GPUVar$global_shared_offset
    
    # worker private data, located in private/local memory
    gpu_lp_data = GPUVar$local_private_data
    gpu_lp_offset = GPUVar$local_private_offset
    
    # worker shared data, located in local memory
    gpu_ls_data = GPUVar$local_shared_data
    gpu_ls_offset = GPUVar$local_shared_offset
    
    gpu_sizeInfo = GPUVar$size_info
    
    gpu_returnSize = GPUVar$return_size
    
    # Deducted variable
    gpu_global_id = GPUVar$gpu_global_id
    gpu_global_size=GPUVar$gpu_global_size
    gpu_worker_offset = GPUVar$worker_offset
    #The distance between matrix elements
    gpu_element_dist=GPUVar$element_dist
    
    # The return variable is also derived, it need the global id to find
    # the private return variable
    gpu_return_variable = GPUVar$return_variable
    
    
    
    var_def_code = c()
    redirect_code=c()
    redirect_var=c()
    gpu_gp_num = -1
    gpu_gs_num = -1
    gpu_lp_num = -1
    gpu_ls_num = -1
    # matrixInd is for finding the index of a matrix size in the gpu code
    varInfo$matrixInd = hash()
    # format: var, precision type, physical length(in byte), row number,
    # col number
    varInfo$matrix_gp = data.frame()
    varInfo$matrix_gs = data.frame()
    varInfo$matrix_lp = data.frame()
    varInfo$matrix_ls = data.frame()
    
    for (curVar in getAllVars(varInfo)) {
        curInfo = getVarInfo(varInfo, curVar, 0)
        curInfo$isPointer=getPointerType(varInfo,curVar)
        
        # if (curInfo$location != "local" || curInfo$shared) 
        #     curInfo$dataType = T_matrix
        
        # If the variable does not need to be initialized 
        # Case 1: the variable is the function argument 
        # Case 2: the variable is a lazy reference
        if (!curInfo$initial_ad) {
          if(isNA(curInfo$address)){
            curInfo$address = curVar
          }
          if(curInfo$redirect!="NA"){
            curCode=paste0("#define ", curVar, " ", curInfo$redirect)
            curSize1=paste0("#define ", getSizeVar(curVar,1)," ",getSizeVar(curInfo$redirect,1))
            curSize2=paste0("#define ", getSizeVar(curVar,2)," ",getSizeVar(curInfo$redirect,2))
            redirect_code=c(redirect_code,curCode,curSize1,curSize2)
            redirect_var=c(redirect_var,curVar)
          }
          # If the variable is the function argument
          if (curInfo$require) {
            gpu_gs_num = gpu_gs_num + 1
            varInfo$matrixInd[[curVar]] = gpu_gs_num
            varInfo$matrix_gs = addvariableSizeInfo(varInfo$matrix_gs, 
                                                    curInfo)
          }
          varInfo = setVarInfo(varInfo, curInfo)
          next
        }
        
        
        
        if (!curInfo$isPointer) {
            CXXtype = getTypeCXXStr(curInfo$precisionType)
            curCode = paste0(CXXtype, " ", curVar, ";")
            var_def_code = rbind(var_def_code,c(curVar, curCode))
            curInfo$address = curVar
            varInfo = setVarInfo(varInfo, curInfo)
            next
        }
        
        if (curInfo$dataType == T_matrix) {
            if (curInfo$location == "global" && curInfo$shared) {
                gpu_gs_num = gpu_gs_num + 1
                varInfo$matrixInd[[curVar]] = gpu_gs_num
                varInfo$matrix_gs = addvariableSizeInfo(varInfo$matrix_gs, 
                  curInfo)
                curCode = addVariableDeclaration(varInfo,curInfo, gpu_gs_data, 
                  gpu_gs_offset, gpu_gs_num,"global")
            }
            if (curInfo$location == "local" && curInfo$shared) {
                gpu_ls_num = gpu_ls_num + 1
                varInfo$matrixInd[[curVar]] = gpu_ls_num
                varInfo$matrix_ls = addvariableSizeInfo(varInfo$matrix_ls, 
                  curInfo)
                curCode = addVariableDeclaration(varInfo,curInfo, gpu_ls_data, 
                  gpu_ls_offset, gpu_ls_num,"local")
            }
            if (curInfo$location == "global" && !curInfo$shared) {
                gpu_gp_num = gpu_gp_num + 1
                varInfo$matrixInd[[curVar]] = gpu_gp_num
                varInfo$matrix_gp = addvariableSizeInfo(varInfo$matrix_gp, 
                  curInfo)
                curCode = addVariableDeclaration(varInfo,curInfo, gpu_gp_data, 
                  gpu_gp_offset, gpu_gp_num,"global")
            }
            # This is wrong..
            if (curInfo$location == "local" && !curInfo$shared) {
                gpu_lp_num = gpu_lp_num + 1
                varInfo$matrixInd[[curVar]] = gpu_lp_num
                varInfo$matrix_lp = addvariableSizeInfo(varInfo$matrix_lp, 
                  curInfo)
                curCode = addVariableDeclaration(varInfo,curInfo, gpu_lp_data, 
                  gpu_lp_offset, gpu_lp_num,"private")
            }
            var_def_code = rbind(var_def_code,c(curVar, curCode$code))
            varInfo=setVarInfo(varInfo,curCode$Info)
            next
        }
    }
    
    
    
    gpu_code = c(
      "//Define some useful macro", 
      paste0("#define ", gpu_worker_offset, " ", gpu_global_id),
      paste0("#define ", gpu_element_dist, " ", gpu_global_id),
      
      "//Data preparation", 
      paste0("size_t ", gpu_global_id, "=get_global_id(0);"),
      "//find the right pointer for the current thread", 
      paste0(gpu_return_variable, "=", gpu_return_variable, "+", gpu_global_id, ";"), 
      "//End of the stage 1 compilation",
      "//Thread number optimization", 
      "//Matrix dimension optimization")
    
    
    
    varInfo$var_def_code=var_def_code
    varInfo$redirect_var=redirect_var
    
    GPUExp1$Exp = parsedExp
    GPUExp1$varInfo = varInfo
    GPUExp1$gpu_code = gpu_code
    GPUExp1$macro=c(GPUExp1$macro,redirect_code)
    
    
    return(GPUExp1)
}


RCcompilerLevel2 <- function(GPUExp1) {
    if (DEBUG) {
        GPUExp1$varInfo = copyVarInfoTbl(GPUExp1$varInfo)
    }
    
    parsedExp = GPUExp1$Exp
    varInfo = GPUExp1$varInfo
    gpu_code = GPUExp1$gpu_code
    gpu_code = c(gpu_code, 
                 "//Start of the GPU code",
                 RCTranslation(varInfo, parsedExp,isTop=TRUE)
                 )
    
    GPUExp2 = GPUExp1
    GPUExp2$gpu_code = gpu_code
    
    GPUExp2
}



RCTranslation <- function(varInfo, parsedExp,isTop=FALSE) {
    gpu_code = c()
    delim=switch(isTop,"\\Main function delimiter",NULL)
    for (i in seq_along(parsedExp)) {
        curExp = parsedExp[[i]]
        if (curExp == "{" || is.symbol(curExp)) 
            next
        gpu_code=c(gpu_code,delim)
        if (curExp[[1]] == "for") {
            curCode = RCTranslation(varInfo, curExp[[4]])
            loopInd = curExp[[2]]
            loopRange = curExp[[3]]
            loopRange_start=C_element_getCExp(varInfo,loopRange[[2]],sub=c(0,0))
            loopRange_end=C_element_getCExp(varInfo,loopRange[[3]],sub=c(0,0),extCode = loopRange_start$extCode)
            
            extCode=unlist(finalizeExtCode(loopRange_end$extCode))
            loopFunc = paste0("for(",
                              loopInd, "=", loopRange_start$value, 
                ";", loopInd, "<=", loopRange_end$value, ";", loopInd, "=",loopInd,"+1){")
            
            if(is.null(extCode)){
              loopState=c(loopFunc, curCode, "}")
            }else{
              loopState=c("{",extCode$L0,
                          loopFunc, curCode, "}","}")
            }
            gpu_code = c(gpu_code,loopState)
            next
        }
        if (curExp[[1]] == "if") {
            curCode = RCTranslation(varInfo, curExp[[3]])
            condition = curExp[[2]]
            condition_C = C_element_getCExp(varInfo, condition, sub=c(0,0))
            extCode = unlist(finalizeExtCode(condition_C$extCode))
            ifFunc = c(paste0("if(", condition_C$value, "){\n"), 
                paste0(curCode, collapse = "\n"), "}")
            elseFunc = NULL
            if (length(curExp) == 4) {
                curCode = RCTranslation(varInfo, curExp[[4]])
                elseFunc = paste0("else{", paste0(curCode, collapse = "\n"), 
                  "}")
            }
            ifstate = c(ifFunc, elseFunc, "}\n")
            if(is.null(extCode)){
              ifstate = c(ifFunc, elseFunc)
            }else{
              ifstate = c("{",extCode,ifFunc, elseFunc,")")
            }
            
            gpu_code = c(gpu_code, ifstate)
            next
        }
        # Add the code tracker
        if(length(grep(GPUVar$preservedFuncPrefix,deparse(curExp[[1]]),fixed = TRUE))==0)
          gpu_code = c(gpu_code, paste0("//", deparse(curExp)))
        # If the code starts with opencl_
        code_char = paste0(deparse(curExp), collapse = "")
        if (substr(code_char, 1, nchar(GPUVar$openclCode)) == GPUVar$openclCode) {
            curCode = paste0(substr(code_char, nchar(GPUVar$openclCode) + 
                1, nchar(code_char)), ";")
            gpu_code = c(gpu_code, curCode)
            next
        }
        if (substr(code_char, 1, nchar(GPUVar$openclFuncCall)) == GPUVar$openclFuncCall) {
            curCode = curExp[[2]]
            if (!is.character(curCode)) {
                curCode = paste0(deparse(curCode), ";")
            }
            gpu_code = c(gpu_code, curCode)
            next
        }
        
        # if the code does not exactly match the template but has an equal
        # sign, partially match is used
        if (curExp[[1]] == "=") {
            curCode = C_assignment_dispatch(varInfo, curExp)
            gpu_code = c(gpu_code, curCode)
            next
        }
        # If not the case above, dispatch the code according to the function
        func = .cFuncs[[deparse(curExp[[1]])]]
        if (!is.null(func)) {
            curCode = func(varInfo, curExp)
            gpu_code = c(gpu_code, curCode)
            next
        }
        
    }
    return(gpu_code)
}

