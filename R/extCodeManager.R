######## Helper functions###################


#' @return A vector of variables
#' @rdname internalFunctions
#' @export
extractVars <- function(x) UseMethod("extractVars")
#' @method extractVars default
#' @rdname internalFunctions
#' @export
extractVars.default <- function(x) {
    matchRes = gregexpr("[a-zA-Z0-9_]+", x)[[1]]
    vars = vapply(seq_along(matchRes), function(i, x, start, len) substr(x, start[i], start[i] + len[i] - 1), "", x, matchRes, 
        attr(matchRes, "match.length"))
    vars
}

#' @method extractVars extCode
#' @rdname internalFunctions
#' @export
extractVars.extCode <- function(x) {
    x$varDef$varName
}
############# extCode definition##############
createExtCode <- function(opt) {
    levelNum = length(opt) + 1
    extCode = structure(list(varDef = NULL, opt = opt, levelNum = levelNum, valueRecord = NULL), class = "extCode")
    for (i in seq_along(opt)) {
        for (j in seq_along(opt[[i]])) {
            extCode = addVarDef_inLevel(extCode, NA, opt[[i]][j], NA, i + 1)
        }
    }
    extCode
}


findVarLevel <- function(extCode, var) {
    ind = which(extCode$varDef$varName %in% var)
    if (length(ind) == 0) 
        level = 1 else level = max(extCode$varDef[ind, ]$level)
    return(level)
}



addVarDef_inLevel <- function(extCode, precision, varName, varDef, level) {
    extCode$varDef = rbind(extCode$varDef, data.frame(precision = precision, varName = varName, varDef = varDef, level = level, 
        stringsAsFactors = FALSE))
    extCode
}

addVarDef <- function(extCode, precision, varName, varDef) {
    vars = extractVars(varDef)
    ind = which(extCode$varDef$varName %in% vars)
    if (length(ind) == 0) 
        level = 1 else level = max(extCode$varDef[ind, ]$level)
    
    extCode = addVarDef_inLevel(extCode, precision, varName, varDef, level)
    extCode
}


finalizeExtCode_hidden <- function(curCode) {
    if (!is.na(curCode$precision)) {
        return(paste0(curCode$precision, " ", curCode$varName, "=", curCode$varDef, ";"))
    } else {
        return(NULL)
    }
}

finalizeExtCode <- function(extCode) {
    levelNum = extCode$levelNum
    value = vector("list", length = levelNum)
    names(value) = paste0("L", seq_len(levelNum) - 1)
    if (!is.null(extCode) && !is.null(extCode$varDef)) {
        for (i in seq_len(nrow(extCode$varDef))) {
            curCode = extCode$varDef[i, ]
            level = curCode$level
            res = finalizeExtCode_hidden(curCode)
            if (!is.null(res)) {
                value[[level]] = c(value[[level]], res)
            }
        }
    }
    value
}
hasVar.extCode <- function(extCode, var) {
    var %in% extCode$varDef$varName
}



getVarFromExtCode <- function(extCode, precision, varDef) {
    ind = which(extCode$varDef$precision == precision & extCode$varDef$varDef == varDef)
    if (length(ind) > 1) {
        stop("Redundant variable definition has been found")
    }
    if (length(ind) == 1) {
        varName = extCode$varDef[ind, "varName"]
    }
    if (length(ind) == 0) {
        varName = GPUVar$getTmpVar()
        extCode = addVarDef(extCode, precision, varName, varDef)
    }
    return(list(var = varName, extCode = extCode))
}

getLevelNum <- function(extCode) {
    extCode$levelNum
}

removeRedundantVar <- function(extCode, var) {
    varNames = extCode$varDef$varName
    newCode = extCode$varDef[varNames != var, ]
    allVarDef = newCode$varDef
    relatedVars = unique(unlist(sapply(allVarDef, extractVars)))
    if (var %in% relatedVars) {
        return(extCode)
    } else {
        extCode$varDef = newCode
        return(extCode)
    }
}
# Add a variable definition record in extra code in a special place The record will not affect the output of
# finalizeExtCode
addValueRecord <- function(extCode, value) {
    extCode$valueRecord = c(extCode$valueRecord, value)
    extCode
}

getAllVarsInRecord <- function(extCode) {
    if (!is.null(extCode$valueRecord)) 
        res = unique(as.vector(unlist(sapply(extCode$valueRecord, extractVars))))
    res
}
# Get the number of variable definition in extra code
getVarsNum <- function(extCode) {
    if (is.null(extCode) || is.null(extCode$varDef)) 
        return(0)
    return(nrow(extCode$varDef))
}


###################### Hoist Optimization################################# opt=list(c('gpu_k1')) extCode=createExtCode(opt)
###################### extCode=addVarDef(extCode,'double','a1','i+j') extCode=addVarDef(extCode,'double','a2','k+j+a3')
###################### extCode=addVarDef(extCode,'double','a3','i+gpu_k1') Exp='gpu_gp_size1_0 * a[(uint)(f+t)]' finalizeExtCode(extCode)




hoistOpt <- function(extCode, Exp) {
    code = C_to_R(Exp)
    code = sapply(expandExp(code), Simplify)
    codeInfo = list()
    baseLevel = c()
    # Decompose the code and find the base level for each code
    for (i in seq_along(code)) {
        codeInfo[[i]] = decomposeCode(extCode, code[i])
        baseLevel = c(baseLevel, max(codeInfo[[i]]$level))
    }
    
    # Upgrade the level if the code is composed by a single variable and its level is unique
    totalLevel = getLevelNum(extCode)
    for (i in seq_len(totalLevel - 1)) {
        ind = which(baseLevel == i)
        if (length(ind) == 1 && nrow(codeInfo[[ind]]) == 1) {
            codeInfo[[ind]]$level = codeInfo[[ind]]$level + 1
            baseLevel[ind] = baseLevel[ind] + 1
        }
    }
    
    baseRes = vector("list", length = totalLevel)
    for (i in seq_along(codeInfo)) {
        curInfo = codeInfo[[i]]
        curlevels = sort(unique(curInfo$level))
        curBase = baseLevel[i]
        for (curLevel in curlevels) {
            if (curLevel != curBase) {
                varDef = CSimplify(constructCode(curInfo, curLevel))
                
                res = getVarFromExtCode(extCode, GPUVar$default_index_type, varDef)
                varName = res$var
                extCode = res$extCode
                curInfo = replaceLevelWithVar(curInfo, varName, curLevel)
            } else {
                baseRes[[curLevel]] = c(baseRes[[curLevel]], constructCode(curInfo, curLevel))
            }
        }
    }
    for (i in seq_along(baseRes)) {
        if (is.null(baseRes[[i]])) 
            next
        if (i != getLevelNum(extCode)) {
            varDef = CSimplify(paste0(baseRes[[i]], collapse = "+"))
            res = getVarFromExtCode(extCode, GPUVar$default_index_type, varDef)
            varName = res$var
            baseRes[[totalLevel]] = c(baseRes[[totalLevel]], varName)
            extCode = res$extCode
        }
    }
    finalRes = list()
    finalRes$value = CSimplify(paste0(baseRes[[totalLevel]], collapse = "+"))
    finalRes$extCode = extCode
    finalRes
}

# Remove the variable which is less than or equal to the given level Add a variable in the given level
replaceLevelWithVar <- function(codeInfo, var, level) {
    ind = which(codeInfo$level <= level)
    codeInfo = codeInfo[-ind, ]
    newVar = data.frame(level = level, var = var, operator = "*", stringsAsFactors = FALSE)
    codeInfo = rbind(codeInfo, newVar)
    codeInfo
}

# Combine the variables into one variable The variables should in the level that is less than or equal to the given
# level
constructCode <- function(codeInfo, level) {
    ind = which(codeInfo$level <= level)
    codeInfo = codeInfo[ind, ]
    res = c()
    for (i in seq_len(length(ind))) {
        curInfo = codeInfo[i, ]
        res = c(res, curInfo$operator, paste0("(", curInfo$var, ")"))
    }
    if (length(res) != 0 && res[1] %in% c("*", "/", "+", "-")) {
        res = res[-1]
    }
    paste0(res, collapse = "")
}

# Decompose the code into different level The code should not be able to separate by +,- operator The current supported
# decompose function is *
decomposeCode <- function(extCode, code) {
    code = decomposeCode_hidden(extCode, code)
    if (nrow(code) > 1) {
        for (i in seq_len(getLevelNum(extCode) - 1)) {
            ind = which(code$level == i)
            if (length(ind) == 1) {
                code[ind, ]$level = i + 1
            }
        }
    }
    code
}
decomposeCode_hidden <- function(extCode, code, operator = "") {
    code = toExpression(code)
    if (is.call(code)) {
        func = deparse(code[[1]])
        if (func == "*") {
            left = decomposeCode_hidden(extCode, code[[2]])
            right = rbind(left, decomposeCode_hidden(extCode, code[[3]], operator = func))
            return(right)
        }
        if (func == "-") {
            res = decomposeCode_hidden(extCode, code[[2]])
            res$var[1] = paste0("-", res$var[1])
            return(res)
        }
        if (func == "(") {
            res = decomposeCode_hidden(extCode, code[[2]])
            return(res)
        }
    }
    
    level = findCodeLevel(extCode, code)
    code_char = deparse(code)
    res = data.frame(level = level, var = code_char, operator = operator, stringsAsFactors = FALSE)
    return(res)
    
}



# Obtain the level of the code
findCodeLevel <- function(extCode, code) {
    code = toCharacter(code)
    vars = extractVars(code)
    level = findVarLevel(extCode, vars)
    return(level)
}

# Expand the parathesis in the expression
expandExp <- function(code) {
    code = toExpression(code)
    if (!is.call(code)) 
        return(deparse(code))
    func = code[[1]]
    if (func == "(") 
        return(expandExp(code[[2]]))
    
    if (deparse(func) %in% c("+", "-", "*")) {
        left = code[[2]]
        right = code[[3]]
        left_exp = expandExp(left)
        right_exp = expandExp(right)
        
        if (func == "+") {
            res = c(left_exp, right_exp)
            return(res)
        }
        if (func == "-") {
            res = c(left_exp, paste0("-", right_exp))
            return(res)
        }
        
        if (func == "*") {
            res = c()
            for (i in seq_along(left_exp)) {
                for (j in seq_along(right_exp)) {
                  res = c(res, paste0(left_exp[i], "*", right_exp[j]))
                }
            }
            return(res)
        }
    }
    return(deparse(code))
}
