#' @include pkgFunc.R

RCcompilerLevel1 <- function(profileMeta2) {
    GPUExp1 = profileMeta2
    if (DEBUG) {
        GPUExp1$varInfo = copyVarInfoTbl(profileMeta2$varInfo)
    }
    parsedExp = GPUExp1$Exp
    varInfo = GPUExp1$varInfo
    
    
    gpu_gp_matrixNum = GPUVar$global_private_matrixNum
    gpu_gs_matrixNum = GPUVar$global_share_matrixNum
    gpu_ls_matrixNum = GPUVar$local_share_matrixNum
    gpu_lp_matrixNum = GPUVar$local_private_matrixNum
    
    gpu_gs_size = GPUVar$global_share_size
    gpu_gp_size = GPUVar$global_private_size
    gpu_ls_size = GPUVar$local_share_size
    
    gpu_gp_transpose = GPUVar$gp_transpose
    gpu_gs_transpose = GPUVar$gs_transpose
    gpu_lp_transpose = GPUVar$lp_transpose
    gpu_ls_transpose = GPUVar$ls_transpose
    
    # Preserved variables Global worker private data
    gpu_gp_data = GPUVar$global_private_data
    # Per worker length
    gpu_gp_totalSize = GPUVar$global_private_totalSize
    
    gpu_gp_size1 = GPUVar$global_private_size1
    gpu_gp_size2 = GPUVar$global_private_size2
    # matrix index offset
    gpu_gp_offset = GPUVar$global_private_offset
    
    # worker shared data, located in global memory
    gpu_gs_data = GPUVar$global_shared_data
    gpu_gs_size1 = GPUVar$global_shared_size1
    gpu_gs_size2 = GPUVar$global_shared_size2
    gpu_gs_offset = GPUVar$global_shared_offset
    
    # worker private data, located in private/local memory
    gpu_lp_data = GPUVar$local_private_data
    gpu_lp_size1 = GPUVar$local_private_size1
    gpu_lp_size2 = GPUVar$local_private_size2
    gpu_lp_offset = GPUVar$local_private_offset
    
    # worker shared data, located in local memory
    gpu_ls_data = GPUVar$local_shared_data
    gpu_ls_size1 = GPUVar$local_shared_size1
    gpu_ls_size2 = GPUVar$local_shared_size2
    gpu_ls_offset = GPUVar$local_shared_offset
    
    gpu_sizeInfo = GPUVar$size_info
    
    gpu_returnSize = GPUVar$return_size
    
    # Deducted variable
    gpu_global_id = GPUVar$gpu_global_id
    gpu_worker_offset = GPUVar$worker_offset
    # The return variable is also derived, it need the global id to find the private return variable
    gpu_return_variable = GPUVar$return_variable
    
    
    
    var_def_code = ""
    gpu_gp_num = -1
    gpu_gs_num = -1
    gpu_lp_num = -1
    gpu_ls_num = -1
    # matrixInd is for finding the index of a matrix size in the gpu code
    varInfo$matrixInd = hash()
    # format: var, precision type, physical length(in byte), row number, col number
    varInfo$matrix_gp = data.frame()
    varInfo$matrix_gs = data.frame()
    varInfo$matrix_lp = data.frame()
    varInfo$matrix_ls = data.frame()
    
    for (curVar in getAllVars(varInfo)) {
        curInfo = getVarInfo(varInfo, curVar, 0)
        
        
        if (curInfo$location != "local" || curInfo$shared) 
            curInfo$dataType = T_matrix
        
        # If the variable does not need to be initialized Case 1: the variable is the function argument Case 2: the variable is
        # a lazy reference
        if (!curInfo$initialization) {
            # Don't touch it, its for the gpu_global_id
            curInfo$address = curInfo$var
            # If the variable is the function argument
            if (curInfo$require) {
                gpu_gs_num = gpu_gs_num + 1
                varInfo$matrixInd[[curVar]] = gpu_gs_num
                curInfo$totalSize = 0
                varInfo$matrix_gs = addvariableSizeInfo(varInfo$matrix_gs, curInfo)
            }
            varInfo = setVarInfo(varInfo, curInfo)
            next
        }
        
        
        
        if (curInfo$dataType == T_scale) {
            CXXtype = curInfo$precisionType
            curCode = paste0(CXXtype, " ", curInfo$var, ";")
            var_def_code = c(var_def_code, curCode)
            curInfo$address = curInfo$var
            varInfo = setVarInfo(varInfo, curInfo)
            next
        }
        
        if (curInfo$dataType == T_matrix) {
            if (curInfo$location == "global" && curInfo$shared) {
                gpu_gs_num = gpu_gs_num + 1
                varInfo$matrixInd[[curVar]] = gpu_gs_num
                varInfo$matrix_gs = addvariableSizeInfo(varInfo$matrix_gs, curInfo)
                curCode = addVariableDeclaration(curInfo, gpu_gs_data, gpu_gs_offset, gpu_gs_num)
            }
            if (curInfo$location == "local" && curInfo$shared) {
                gpu_ls_num = gpu_ls_num + 1
                varInfo$matrixInd[[curVar]] = gpu_ls_num
                varInfo$matrix_ls = addvariableSizeInfo(varInfo$matrix_ls, curInfo)
                curCode = addVariableDeclaration(curInfo, gpu_ls_data, gpu_ls_offset, gpu_ls_num)
            }
            if (curInfo$location == "global" && !curInfo$shared) {
                gpu_gp_num = gpu_gp_num + 1
                varInfo$matrixInd[[curVar]] = gpu_gp_num
                varInfo$matrix_gp = addvariableSizeInfo(varInfo$matrix_gp, curInfo)
                curCode = addVariableDeclaration(curInfo, gpu_gp_data, gpu_gp_offset, gpu_gp_num)
            }
            # This is wrong..
            if (curInfo$location == "local" && !curInfo$shared) {
                gpu_lp_num = gpu_lp_num + 1
                varInfo$matrixInd[[curVar]] = gpu_lp_num
                varInfo$matrix_lp = addvariableSizeInfo(varInfo$matrix_lp, curInfo)
                curCode = addVariableDeclaration(curInfo, gpu_lp_data, gpu_lp_offset, gpu_lp_num)
            }
            curInfo$address = curInfo$var
            var_def_code = c(var_def_code, curCode)
            varInfo = setVarInfo(varInfo, curInfo)
            next
        }
        
    }
    
    
    gpu_code = c("//Define some useful macro", paste0("#define ", gpu_gp_totalSize, " ", gpu_sizeInfo, "[0]"), paste0("#define ", 
        gpu_returnSize, " ", gpu_sizeInfo, "[1]"), paste0("#define ", gpu_gp_matrixNum, " ", nrow(varInfo$matrix_gp)), paste0("#define ", 
        gpu_gs_matrixNum, " ", nrow(varInfo$matrix_gs)), paste0("#define ", gpu_ls_matrixNum, " ", nrow(varInfo$matrix_ls)), 
        paste0("#define ", gpu_lp_matrixNum, " ", nrow(varInfo$matrix_lp)), paste0("#define ", gpu_worker_offset, " ", gpu_global_id, 
            "*", gpu_gp_totalSize), "//Data preparation", paste0("size_t ", gpu_global_id, "=get_global_id(0);"), paste0("global ", 
            GPUVar$default_index_type, "* ", gpu_gp_size1, "=", gpu_gp_size, ";"), paste0("global ", GPUVar$default_index_type, 
            "* ", gpu_gp_size2, "=", gpu_gp_size, "+", gpu_gp_matrixNum, ";"), paste0("global ", GPUVar$default_index_type, 
            "* ", gpu_gs_size1, "=", gpu_gs_size, ";"), paste0("global ", GPUVar$default_index_type, "* ", gpu_gs_size2, 
            "=", gpu_gs_size, "+", gpu_gs_matrixNum, ";"), paste0("global ", GPUVar$default_index_type, "* ", gpu_ls_size1, 
            "=", gpu_ls_size, ";"), paste0("global ", GPUVar$default_index_type, "* ", gpu_ls_size2, "=", gpu_ls_size, "+", 
            gpu_ls_matrixNum, ";"), "//find the right pointer for the current thread", paste0(gpu_gp_data, "=", gpu_gp_data, 
            "+", gpu_worker_offset, ";"), paste0(gpu_return_variable, "=", gpu_return_variable, "+", gpu_returnSize, "*", 
            gpu_global_id, ";"), "//variable definitions", var_def_code, "//End of the stage 1 compilation", "//Thread number optimization", 
        "//Matrix dimension optimization")
    
    
    GPUExp1$Exp = parsedExp
    GPUExp1$varInfo = varInfo
    GPUExp1$gpu_code = gpu_code
    
    
    return(GPUExp1)
}


RCcompilerLevel2 <- function(GPUExp1) {
    if (DEBUG) {
        GPUExp1$varInfo = copyVarInfoTbl(GPUExp1$varInfo)
        if (length(GPUExp1$varInfo$matrixInd) != 0) 
            GPUExp1$varInfo$matrixInd = copy(GPUExp1$varInfo$matrixInd)
    }
    
    parsedExp = GPUExp1$Exp
    varInfo = GPUExp1$varInfo
    gpu_code = GPUExp1$gpu_code
    gpu_code = c(gpu_code, "//Start of the GPU code", RCTranslation(varInfo, parsedExp))
    
    GPUExp2 = GPUExp1
    GPUExp2$gpu_code = gpu_code
    
    GPUExp2
}



RCTranslation <- function(varInfo, parsedExp) {
    gpu_code = c()
    for (i in seq_along(parsedExp)) {
        curExp = parsedExp[[i]]
        if (curExp == "{" || is.symbol(curExp)) 
            next
        
        
        
        if (curExp[[1]] == "for") {
            curCode = RCTranslation(varInfo, curExp[[4]])
            loopInd = curExp[[2]]
            loopRange = curExp[[3]]
            curVar = getVarInfo(varInfo, loopInd)
            CXXtype = curVar$precisionType
            loopFunc = paste0("for(", CXXtype, " ", loopInd, "=", loopRange[[2]], ";", loopInd, "<=", loopRange[[3]], ";", 
                loopInd, "++){")
            gpu_code = c(gpu_code, loopFunc, curCode, "}")
            next
        }
        if (curExp[[1]] == "if") {
            curCode = RCTranslation(varInfo, curExp[[3]])
            condition = curExp[[2]]
            condition_C = R_expression_sub(varInfo, condition, 1)
            extCode = unlist(finalizeExtCode(condition_C$extCode))
            ifFunc = c("{", extCode, paste0("if(", condition_C$value, "){\n"), paste0(curCode, collapse = "\n"), "}")
            elseFunc = NULL
            if (length(curExp) == 4) {
                curCode = RCTranslation(varInfo, curExp[[4]])
                elseFunc = paste0("else{", paste0(curCode, collapse = "\n"), "}")
            }
            ifstate = c(ifFunc, elseFunc, "}\n")
            gpu_code = c(gpu_code, ifstate)
            next
        }
        # Add the code tracker
        gpu_code = c(gpu_code, paste0("//", deparse(curExp)))
        # If the code starts with opencl_
        code_char = paste0(deparse(curExp), collapse = "")
        if (substr(code_char, 1, nchar(GPUVar$openclCode)) == GPUVar$openclCode) {
            curCode = paste0(substr(code_char, nchar(GPUVar$openclCode) + 1, nchar(code_char)), ";")
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
        
        # if the code format exactly match the template
        formattedExp = formatCall(curExp, generalType = FALSE)
        formattedExp_char = gsub(" ", "", deparse(formattedExp), fixed = TRUE)
        func = .cFuncs[[formattedExp_char]]
        if (!is.null(func)) {
            curCode = func(varInfo, curExp)
            gpu_code = c(gpu_code, curCode)
            next
        }
        
        # if the code format exactly match the general type template
        formattedExp = formatCall(curExp, generalType = TRUE)
        formattedExp_char = gsub(" ", "", deparse(formattedExp), fixed = TRUE)
        func = .cFuncs[[formattedExp_char]]
        if (!is.null(func)) {
            curCode = func(varInfo, curExp)
            gpu_code = c(gpu_code, curCode)
            next
        }
        
        
        # if the code does not exactly match the template but has an equal sign, partially match is used
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

