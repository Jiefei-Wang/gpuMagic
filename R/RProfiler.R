#' @include pkgFunc.R
# Functions: 1.Profile the function argument

RProfile1 <- function(codeMetaInfo2) {
    profileMeta1 = codeMetaInfo2
    
    # The looped variable needs some special treatment, because inside each
    # function it has a unique looped variable The overall looped variable
    # is redefined to distinguish the overall and individual looped
    # variables.
    loopData = names(profileMeta1$parms)[1]
    func_args = profileMeta1$parms
    
    # profile the function arguments and the preserved variable
    varInfo = profileVar(func_args, profileMeta1$parmsWithValue, profileMeta1$parmsConst)
    gpuIndex = getEmpyTable(type = T_scale)
    gpuIndex$var = GPUVar$gpu_global_id
    gpuIndex$precisionType = GPUVar$default_index_type
    gpuIndex$initial_ad = FALSE
    varInfo = addVarInfo(varInfo, gpuIndex)
    
    profileMeta1$varInfo = varInfo
    
    profileMeta1
}



# Function: 1. Profile the variable type 2. rename the variable if the
# type does not match
RProfile2 <- function(profileMeta1) {
    if (DEBUG) {
        profileMeta1$varInfo = copyVarInfoTbl(profileMeta1$varInfo)
    }
    profileMeta1$varInfo$errorCheck = c()
    
    profileMeta2 = parserFrame(RProfile2_parserFunc, RProfile2_checkFunc, 
        RProfile2_updateFunc, profileMeta1)
    
    profileMeta2
}

RProfile2_parserFunc <- function(level, codeMetaInfo, curExp) {
    result = list(Exp = curExp)
    varInfo = codeMetaInfo$varInfo
    renameList = c()
    result$Exp = curExp
    result$varInfo = varInfo
    
    
    formattedExp = formatCall(curExp, generalType = FALSE)
    formattedExp_char = gsub(" ", "", deparse(formattedExp), fixed = TRUE)
    # process transpose
    if (formattedExp_char == "var=t(var)") {
        if (curExp[[2]] == curExp[[3]][[2]]) {
          result=profile_selfTranspose(varInfo,curExp)
          return(result)
        }
    }
    # stop when the code is like A=B%*%A, it is unsafe to do the operation
    # and assign back the result to the original matrix THIS NEEDS TO BE
    # MOVED TO PARSER####################
    if (formattedExp_char == "var=var%*%var") {
        if (curExp[[2]] == curExp[[3]][[3]]) {
            stop("The the left variable cannot be the same as the right variable:\n", 
                deparse(curExp))
        }
    }
    
    
    
    if (curExp[[1]] == "=") {
        leftExp = curExp[[2]]
        if (is.symbol(leftExp)) {
            # profile the left symbol
            res = profiler_assignment_dispatch(level, varInfo, curExp)
            for (i in names(res)) {
                result[[i]] = res[[i]]
            }
            return(result)
        }
    }
    
    
    
    # For the no copy return, the function will first try to redirect it to
    # the return variable in the function argument. If it is not possible,
    # the legacy method will be used.
    if (curExp[[1]] == "return.nocpy") {
        returnVar = deparse(curExp[[2]])
        returnInfo = redirectVar(varInfo, returnVar, GPUVar$return_variable)
        if (returnInfo$redirect == GPUVar$return_variable) {
            setVarInfo(varInfo, returnInfo)
            varInfo$returnInfo = rbind(varInfo$returnInfo, returnInfo)
            curExp = quote(return())
        } else {
            curExp[[1]] = as.symbol("return")
        }
    }
    
    
    if (curExp[[1]] == "return" && length(curExp) > 1) {
        returnVar = deparse(curExp[[2]])
        returnInfo = getVarInfo(varInfo, returnVar)
        varInfo$returnInfo = rbind(varInfo$returnInfo, returnInfo)
    }
    
    if(curExp[[1]] == "compiler.release"){
      for(i in seq_len(length(curExp)-1)+1){
        varInfo=release_var(varInfo,curExp[[i]])
      }
    }
    
    
    result$Exp = curExp
    result$renameList = renameList
    result$varInfo = varInfo
    return(result)
}


# Exp='b*a[1,10]+c(4,43)[1]+10-1' Simplify(Exp)

# Filter the preserved functions
RProfile2_checkFunc <- function(curExp) {
    if (!is.call(curExp)) {
        return(FALSE)
    }
    if (is.preservedFunc(curExp)) 
        return(FALSE)
    return(TRUE)
}

RProfile2_updateFunc <- function(type, level, codeMetaInfo, parsedExp, 
    code, i, res) {
    result = general_updateFunc(codeMetaInfo, parsedExp, code)
    result$codeMetaInfo$varInfo = res$varInfo
    result$codeMetaInfo$errorCheck = rbind(result$codeMetaInfo$errorCheck, 
        res$errorCheck)
    result
}


