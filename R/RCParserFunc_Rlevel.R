# Fill the target vector with default value to reach the desinged
# length
fillDefauleValue <- function(target, designLen, default) {
    if (designLen == length(target)) {
        return(target)
    }
    if (designLen > length(target)) {
        target_new = rep(default, designLen)
        target_new[seq_len(designLen)] = target
    } else {
        target_new = target[seq_len(designLen)]
    }
    target_new
}
# R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),c(3,6),opt=NULL)
# R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),3,opt=F)
# R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),c('k1',6),sub_C=TRUE,opt=NULL)
# R_expression_sub(varInfo,quote(gpu_temp_var2[C,]),c('k1',6),sub_C=TRUE,opt=list('k1'))
# R_expression_sub(varInfo,quote(C[C,]),c('k1',6),sub_C=TRUE,opt=NULL)
# R_expression_sub(varInfo,quote(C[C,]),c('k1',6),sub_C=TRUE,opt=list('k1'))
R_expression_sub <- function(varInfo, Exp, sub, sub_C = FALSE, opt = NULL, 
    extCode = NULL, base = 1) {
    # fill the vector with the first element to make the length of it
    # consistant with the length of Exp
    sub_C = fillDefauleValue(sub_C, length(sub), sub_C[1])
    if (is.null(extCode)) 
        extCode = createExtCode(opt)
    
    if (length(sub) == 1) {
        res = R_oneIndex_exp_sub(varInfo, Exp, sub, sub_C, opt, extCode, 
            base)
    } else {
        res = R_twoIndex_exp_sub(varInfo, Exp, sub[1], sub[2], sub_C[1], 
            sub_C[2], opt, extCode, base)
    }
    
    extCode = res$extCode
    extCode = addValueRecord(extCode, res$value)
    curVarNum = length(extractVars(extCode)) + 1
    while (curVarNum != length(extractVars(extCode))) {
        curVarNum = length(extractVars(extCode))
        vars = getAllVarsInRecord(extCode)
        tmpVars = extractVars(extCode)
        for (i in seq_along(tmpVars)) {
            if (!tmpVars[i] %in% vars) {
                extCode = removeRedundantVar(extCode, tmpVars[i])
            }
        }
    }
    res$extCode = extCode
    
    return(res)
}


# Expression should be a variable or a matrix subset
# R_oneIndex_exp_sub(varInfo,quote(A[tmp1]),3)
R_oneIndex_exp_sub <- function(varInfo, Exp, k, k_C = FALSE, opt = NULL, 
    extCode = NULL, base = 1) {
    if (is.null(extCode)) 
        extCode = createExtCode(opt)
    k = paste0(k, "+", 1 - base)
    k = CSimplify(k, k_C)
    
    
    
    # If the expression is empty, return the k index.
    if (Exp == "") {
        k_C_ind = wrapIndex(varInfo, k, k_C, extCode, opt)
        return(list(value = k_C_ind$value, extCode = k_C_ind$extCode))
    }
    # Simplify the expression to make sure no 1+0 cases
    Exp = Simplify(Exp)
    # Check if the result is a number
    if (isNumeric(Exp)) {
        return(list(value = toCharacter(Exp), extCode = extCode))
    }
    # Convert the character to the expression
    Exp = toExpression(Exp)
    
    # If the expression is a variable
    if (is.symbol(Exp)) {
        curInfo = getVarInfo(varInfo, Exp)
        if (curInfo$isSeq) {
            seqInfo = getSeqAddress(varInfo, Exp)
            
            k_C_ind = wrapIndex(varInfo, k, k_C, extCode, opt)
            res = list()
            res$extCode = k_C_ind$extCode
            res$value = CSimplify(paste0(seqInfo$from, "+", "(", k_C_ind$value, 
                "-1)*", seqInfo$by))
            return(res)
        }
        
        k_C_ind = wrapIndex(varInfo, k, k_C, extCode, opt)
        res = oneIndex_to_twoIndex(varInfo, Exp, k_C_ind$value, rowNum = R_getVarSize1(varInfo, 
            Exp), opt = opt, extCode = k_C_ind$extCode)
        return(res)
    }
    # if the expression is a subset of a matrix
    if (Exp[[1]] == "[") {
        curVar = Exp[[2]]
        args = matchBracketFunc(Exp)
        # if it is one index subset
        if (is.null(args$j)) {
            sub = args$i
            k_C_ind = R_oneIndex_exp_sub(varInfo, sub, k = k, k_C = k_C, 
                opt = opt, extCode = extCode)
            res = R_oneIndex_exp_sub(varInfo, curVar, k = k_C_ind$value, 
                k_C = TRUE, opt = opt, extCode = extCode)
            return(res)
        } else {
            size = R_nrow(varInfo, Exp)
            k_C_ind = wrapIndex(varInfo, k, k_C, extCode, opt)
            res = oneIndex_to_twoIndex(varInfo, Exp, k_C_ind$value, rowNum = size, 
                opt = opt, extCode = k_C_ind$extCode)
            return(res)
            
        }
    }
    stop("unrecognized code: ", deparse(Exp))
}
# assume 1-based index
oneIndex_to_twoIndex <- function(varInfo, Exp, k_C_value, rowNum, opt = NULL, 
    extCode = NULL) {
    tmpVar = GPUVar$getTmpVar()
    tmpVar_value = CSimplify(paste0("(", GPUVar$default_index_type, ")((", 
        k_C_value, "-1)/(", rowNum, "))"))
    # if the temporary variable is a constant, it will be plug into the
    # code Otherwise, create a temporary variable for it
    if (isNumeric(tmpVar_value) || length(grep("/", tmpVar_value, fixed = TRUE)) == 
        0) {
        tmpVar = paste0("(", tmpVar_value, ")")
    } else {
        res = getVarFromExtCode(extCode, GPUVar$default_index_type, tmpVar_value)
        tmpVar = res$var
        extCode = res$extCode
    }
    
    i_C_ind = paste0(k_C_value, "-", rowNum, "*", tmpVar)
    j_C_ind = paste0(tmpVar, "+1")
    
    res = R_twoIndex_exp_sub(varInfo, Exp, i = i_C_ind, j = j_C_ind, i_C = TRUE, 
        j_C = TRUE, opt = opt, extCode = extCode)
    
    
    return(res)
}


# get the i,jth element from the expression, the expression can be a
# variable, a matrix subset or a number
# i,j can be interpreted as an R
# object or a C object, it is determined by i_C and j_C 
# Special case:
# Exp can be empty, then i will be returned Exp can be a value, then
# the value will be returned
# Exp can be a scalar, then the value will be returned 
# R_twoIndex_exp_sub(varInfo,quote(gpu_temp_var2[C,]),3,6)
R_twoIndex_exp_sub <- function(varInfo, Exp, i, j = 1, i_C = FALSE, j_C = FALSE, 
    opt = FALSE, extCode = NULL, base = 1) {
    if (is.null(extCode)) 
        extCode = createExtCode(opt)
    # Convert all the 0-based index to 1-based index
    i = paste0(i, "+", 1 - base)
    j = paste0(j, "+", 1 - base)
    base = 1
    i = CSimplify(i, i_C)
    j = CSimplify(j, j_C)
    
    # If the expression is empty, return the i index.
    if (Exp == "") {
        if (isNumeric(j) && as.character(j) != "1") 
            stop("Incorrect subset index")
        
        i_C_ind = wrapIndex(varInfo, i, i_C, extCode, opt)
        return(list(value = i_C_ind$value, extCode = i_C_ind$extCode))
    }
    # Simplify the expression to make sure no 1+0 cases
    Exp = Simplify(Exp)
    # Check if the result is a number
    if (isNumeric(Exp)) {
        return(list(value = toCharacter(Exp), extCode = extCode))
    }
    # Convert the character to the expression
    Exp = toExpression(Exp)
    
    # if the expression contains only one element
    if (length(Exp) == 1) {
        curVar = deparse(Exp)
        curInfo = getVarInfo(varInfo, curVar)
        # If the expression is a lazy reference
        if (curInfo$isRef) {
            refExp = parse(text = curInfo$specialContent)[[1]]
            if (curInfo$transpose) {
                res = R_twoIndex_exp_sub(varInfo, refExp, j, i, i_C = j_C, 
                  j_C = i_C, opt = opt, extCode = extCode, base = base)
            } else {
                res = R_twoIndex_exp_sub(varInfo, refExp, i, j, i_C = i_C, 
                  j_C = j_C, opt = opt, extCode = extCode, base = base)
            }
            return(res)
        }
        
        if (curInfo$isSeq) {
            
            seqInfo = getSeqAddress(varInfo, curVar)
            
            i_C_ind = wrapIndex(varInfo, i, i_C, extCode, opt)
            j_C_ind = wrapIndex(varInfo, j, j_C, i_C_ind$extCode, opt)
            rowNum = R_getVarSize1(varInfo, Exp)
            res = list()
            res$extCode = j_C_ind$extCode
            res$value = CSimplify(paste0(seqInfo$from, "+", "(", i_C_ind$value, 
                "+", rowNum, "*(", j_C_ind$value, "-1)-1)*", seqInfo$by))
            return(res)
        }
        # If the expression is just a variable
        dataType = curInfo$dataType
        # Scalar
        if (dataType == T_scale) 
            return(list(value = curInfo$address, extCode = extCode))
        # Matrix
        if (dataType == T_matrix) {
            
            i_C_ind = wrapIndex(varInfo, i, i_C, extCode, opt)
            j_C_ind = wrapIndex(varInfo, j, j_C, i_C_ind$extCode, opt)
            res = R_getVarSub(varInfo, Exp, i_C_ind$value, j_C_ind$value, 
                opt = opt, extCode = j_C_ind$extCode)
            return(res)
        }
        stop("unrecognized code: ", deparse(Exp))
    }
    # If the expression is also a subset of a matrix
    if (Exp[[1]] == "[") {
        curVar = Exp[[2]]
        args = matchBracketFunc(Exp)
        if (is.null(args$j)) {
            ref_k = args$i
            ref_k_C = R_oneIndex_exp_sub(varInfo, ref_k, k = i, k_C = i_C, 
                extCode = extCode)
            res = R_oneIndex_exp_sub(varInfo, curVar, k = ref_k_C$value, 
                k_C = TRUE, opt = opt, extCode = ref_k_C$extCode)
        } else {
            ref_i = args$i
            ref_j = args$j
            ref_i_C = R_oneIndex_exp_sub(varInfo, ref_i, k = i, k_C = i_C, 
                opt = opt, extCode = extCode)
            ref_j_C = R_oneIndex_exp_sub(varInfo, ref_j, k = j, k_C = j_C, 
                opt = opt, extCode = ref_i_C$extCode)
            res = R_twoIndex_exp_sub(varInfo, curVar, i = ref_i_C$value, 
                j = ref_j_C$value, i_C = TRUE, j_C = TRUE, opt = opt, extCode = ref_j_C$extCode)
        }
        return(res)
    }
    stop("unrecognized code: ", deparse(Exp))
}
# Return the first element in the argument i If the index is a c
# variale, direct return it as a list If the index is an R variable,
# use subset code to process it
wrapIndex <- function(varInfo, i, i_C, extCode, opt) {
    if (!i_C) {
        res = R_twoIndex_exp_sub(varInfo, Exp = i, i = 1, j = 1, extCode = extCode, 
            opt = opt)
    } else {
        res = list(value = i, extCode = extCode)
    }
    res
}

# Get an element from the matrix(eg. A[i,j]), the transpose will be
# taken into account i,j is 1-based index by default i,j should be
# either a number or a variable in C code If opt=FALSE, the c code will
# be returned If pt=TRUE, a list will be returned. list element: C_sub,
# rowOffset, colOffset optCode should be a list with rowVar,and colVar
# as the elements. Both element can be optional
R_getVarSub <- function(varInfo, var, i, j = 1, opt = NULL, extCode = NULL, 
    base = 1) {
    var = toCharacter(var)
    
    if (var == "") 
        stop("something is wrong..")
    # return(list(value=toCharacter(i),extCode=extCode))
    
    curInfo = getVarInfo(varInfo, var)
    address = curInfo$address
    transpose = curInfo$transpose
    isPointer=curInfo$isPointer
    
    #If the data is not a pointer, it is a scalar, then directly return the address
    if(is.na(isPointer)) stop("Unable to determine the type of the variable,",
                              " this is a bug in the package, please contact the author.")
    if(!isPointer) 
      return(list(value =address,extCode=extCode))
    #If the data is a pointer, but the size is 1, then return the first element in the address
    if(curInfo$size1=="1"&&curInfo$size2=="1")
      return(list(value =paste0(address,"[0]"),extCode=extCode))
    
    
    # Get the simplified index
    if (isNumeric(i)) {
        sub1 = Simplify(paste0(i, "-", base))
    } else {
        sub1 = paste0(i, "-", base)
    }
    
    if (isNumeric(j)) {
        sub2 = Simplify(paste0(j, "-", base))
    } else {
        sub2 = paste0(j, "-", base)
    }
    
    # compute the matrix offset
    size1 = R_getVarSize1(varInfo, var)
    
    if (transpose) {
        tmp = sub1
        sub1 = sub2
        sub2 = tmp
    }
    
    rowOffset = sub1
    colOffset = paste0("(", sub2, ")*", size1)
    
    
    offset = CSimplify(paste0(rowOffset, "+", colOffset))
    if (!is.null(opt)) {
        res = hoistOpt(extCode, offset)
        offset = res$value
        extCode = res$extCode
    }
    c_sub = R_C_Sub(address, offset, simplification = TRUE)
    
    return(list(value = c_sub, extCode = extCode))
}

# The C subset: var[offset] All the argument should be available in C
# code
R_C_Sub <- function(var, offset, simplification = FALSE) {
    res = paste0(var, "[(", GPUVar$default_index_type, ")(", offset, ")]")
    if (simplification) 
        res = CSimplify(res, TRUE)
    return(res)
}

# Get the number of rows for a matrix in C format
R_nrow <- function(varInfo, var,C_symbol=FALSE) {
    var_char = toCharacter(var)
    var = toExpression(var)
    if (isNumeric(var_char)) 
        return(1)
    if (is.call(var)) 
        return(R_getVarSize1(varInfo, var,C_symbol))
    
    curInfo = getVarInfo(varInfo, var)
    ifelse(curInfo$transpose, 
           R_getVarSize2(varInfo, var,C_symbol), 
           R_getVarSize1(varInfo, var,C_symbol)
           )
}
# Get the number of rows for a matrix in C format
R_ncol <- function(varInfo, var,C_symbol=FALSE) {
    var_char = toCharacter(var)
    var = toExpression(var)
    if (isNumeric(var_char)) 
        return(1)
    if (is.call(var)) 
        return(R_getVarSize2(varInfo, var,C_symbol))
    
    curInfo = getVarInfo(varInfo, var)
    ifelse(curInfo$transpose, 
           R_getVarSize1(varInfo, var,C_symbol), 
           R_getVarSize2(varInfo, var,C_symbol)
           )
}
R_length <- function(varInfo, var,C_symbol=FALSE) {
    return(CSimplify(paste0(R_nrow(varInfo, var,C_symbol), "*", R_ncol(varInfo, 
        var,C_symbol))))
    
}


R_getVarSize <- function(varInfo, var,C_symbol, ind) {
    var_char = toCharacter(var)
    var = toExpression(var)
    
    # Detect if the variable is a subset of a matrix Or a lazy reference
    Exp = NULL
    if (is.call(var)) {
      if(var[[1]] == "["){
        Exp = var
      }else{
        #If the function is an element operation
        #If the size are all 1, return 1
        #If one matrix has non-1 size, return the dimension of that matrix
        #If multiple matrix has non-1 size, return the maximum among them
        vars=extractVars(var)
        sizeList=sapply(vars,R_getVarSize,varInfo=varInfo,C_symbol=C_symbol,ind=ind)
        sizeList_notOne=sizeList[sizeList!="1"]
        if(length(sizeList_notOne)==0) {
          return(1)
        }
        if(length(sizeList_notOne)==1){
          return(sizeList_notOne)
          }
        numInd_logic=vapply(sizeList_notOne, isNumeric, FUN.VALUE = logical(1))
        numInd=which(numInd_logic)
        if(length(numInd)!=0){
          return(sizeList_notOne[numInd[1]])
        }
        #length(sizeList_notOne)>1
        #All variables are not number
        res=paste0("max(",sizeList_notOne[1],",",sizeList_notOne[2],")")
        for(i in seq_len(length(sizeList_notOne)-2)+2){
          res=paste0("max(",sizeList_notOne[i],",",res,")")
        }
        return(res)
      }
    } 
    
    if(is.null(Exp)){
      curInfo = getVarInfo(varInfo, var)
      if (curInfo$isRef) {
        Exp = parse(text = curInfo$specialContent)[[1]]
      }
    }
    # If the variable is a subset of a matrix
    if (!is.null(Exp)) {
        args = matchBracketFunc(Exp)
        refVar = Exp[[2]]
        if (ind == 1) {
            # If the first index is empty. eg: A[,1]
            if (!is.null(args$i) && args$i == "") 
                return(R_nrow(varInfo, refVar,C_symbol))
            
            sub1 = args$i
            if (isNumeric(sub1)) 
                return(1)
            
            return(R_length(varInfo, sub1,C_symbol))
        }
        if (ind == 2) {
            # If the second index is empty. eg: A[1,]
            if (!is.null(args$j) && args$j == "") 
                return(R_ncol(varInfo, refVar,C_symbol))
            # If there is no second index. eg:A[1]
            if (is.null(args$j)) 
                return(1)
            
            subs = args$j
            if (isNumeric(subs)) 
                return(1)
            
            return(R_length(varInfo, subs,C_symbol))
        }
    }
    
    
    # If the variable is just a variable and is not a lazy reference
    # curInfo is obtained above 
    # curInfo=getVarInfo(varInfo,var,1)
    
    
    
    
    if (curInfo$dataType == T_scale) 
        return(1)
    
    if(!C_symbol){
      size=switch(ind,curInfo$size1,curInfo$size2)
      if(isNumeric(size))
        return(size)
    }
    
    var_ind = varInfo$matrixInd[[var_char]]
    if (var_ind == "" || is.na(var_ind)) 
        stop("Error in finding the matrix size")
    loc = NA
    if (curInfo$location == "global" && !curInfo$shared) 
        loc = "global_private_"
    if (curInfo$location == "global" && curInfo$shared) 
        loc = "global_shared_"
    if (curInfo$location == "local" && !curInfo$shared) 
        loc = "local_private_"
    if (curInfo$location == "local" && curInfo$shared) 
        loc = "local_shared_"
    if (is.na(loc)) 
        stop("undetermined matrix property!")
    
    
    size = paste0(GPUVar[[paste0(loc, "size", ind)]], "[", var_ind, "]")
    
    size
}
# get the variable row number in C code format Use R_nrow instead, this
# function does not take the transpose into account
R_getVarSize1 <- function(varInfo, var,C_symbol=FALSE) {
    R_getVarSize(varInfo, var,C_symbol, 1)
}
# get the variable column number in C code format Use R_ncol instead,
# this function does not take the transpose into account
R_getVarSize2 <- function(varInfo, var,C_symbol=FALSE) {
    R_getVarSize(varInfo, var,C_symbol, 2)
}
