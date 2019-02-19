# ==============================parser 1==========================
# simplifySingleCode(quote({a=floor(r[2]+1)+d})[[2]])
# simplifySingleCode(quote({a=a+b+sum(colSums(a)+b)})[[2]]) 
# simplifySingleCode(quote({a=a+nrow(a[10])})[[2]]) 
# simplifySingleCode(quote({a[1]=2})[[2]]) 
# return: 
# Exp, insertBefore, insertAfter , insertInMainBefore, insertInMainAfter

simplifySingleCode <- function(Exp) {
    leftExp = removeExpParenthesis(Exp[[2]])
    rightExp = removeExpParenthesis(Exp[[3]])
    Exp[[2]]=leftExp
    Exp[[3]]=rightExp
    ExpOp = Exp[[1]]
    # If the left expression is a matrix subset, replace it with a new
    # variable Otherwise only replace the function argument if needed
    if (is.call(leftExp)) {
      if(!paste0(leftExp[[1]],"<-") %in% names(.cFuncs))
        stop("Unsupported left expression: ", deparse(Exp))
    }
    res_left = simplifyElementOp(leftExp, useElementOp = TRUE, isTop = TRUE)
    # For the right expression, check if the function is an element-wise operation
    # If not, create a variable to replace it right expression
    res_right = simplifyElementOp(rightExp, useElementOp = TRUE, isTop = TRUE)
    
    #Combine the left and right result
    result = list(Exp=Exp)
    result=combineInsertCode(result,res_left,res_right)
    
    return(result)
}
#simplifySingleCode(quote({a=colSums(a)+b})[[2]]) 


# Simplify the expression from one side of = sign 
# useElementOp: if the element operation will be preserved 
# isTop: whether the function is called by other function or called by itself 
# return: Exp, extCode
# simplifyElementOp(quote(return(a+b)),TRUE,TRUE)
simplifyElementOp <- function(Exp, useElementOp = TRUE, isTop = TRUE) {
  
    result = list(Exp=Exp)
    if (!is.call(Exp)) 
        return(list(Exp = Exp))
    curFun = deparse(Exp[[1]])
    # Check if the expression start with the element opration
    if (useElementOp && curFun %in% c(.elementOp, .noParentElementOP, .noChildElementOP)) {
        if (curFun %in% .noParentElementOP) {
            if (!isTop) {
                res = replaceCode(Exp)
                return(res)
            }
        }
        
        if (curFun %in% .noChildElementOP) {
            child_eleOP = FALSE
        }else{
            child_eleOP = TRUE
        }
        for (i in seq_len(length(Exp) - 1) + 1) {
            curExp = Exp[[i]]
            res = simplifyElementOp(curExp, useElementOp = child_eleOP, isTop = FALSE)
            result=combineInsertCode(result,res,offset=i,autoOffset=FALSE)
        }
        return(result)
    }
    # If the expression is a part of a larger expression and the function is not an element operation, Replace the whole
    # expression
    if (!isTop) {
        res = replaceCode(Exp)
        return(res)
    } else {
        for (i in seq_len(length(Exp) - 1) + 1) {
            curExp = Exp[[i]]
            if (deparse(Exp[[i]]) != "" && is.call(curExp)) {
                res = replaceCode(curExp)
                result=combineInsertCode(result,res,offset=i,autoOffset=FALSE)
            }
        }
      return(result)
    }
    stop("This should not happens")
}

# Replace the expression with a variable 
# return extCode,varName
replaceCode <- function(Exp) {
    if (!is.call(Exp)) 
        return(list(varName = Exp))
    varName = GPUVar$getTmpVar()
    if (Exp[[1]] == "[") {
        subsetArgs = matchBracketFunc(Exp)
        subsetArgs$i=toCharacter(subsetArgs$i)
        if (is.null(subsetArgs$j)) {
            replaceCode = paste0(varName, "=subRef(", Exp[[2]], ",", subsetArgs$i,
                ")")
        } else {
          subsetArgs$j=toCharacter(subsetArgs$j)
            replaceCode = paste0(varName, "=subRef(", Exp[[2]], ",", subsetArgs$i,
                ",", subsetArgs$j, ")")
        }
    } else {
        replaceCode = paste0(varName, "=", deparse(Exp))
    }
    
    #replaceCode = paste0(varName, "=", deparse(Exp))
    
    replaceCode = parse(text = replaceCode)[[1]]
    releaseCode=parse(text=paste0("compiler.release(",varName,")"))[[1]]
    # Further simplify the code if needed
    res = simplifySingleCode(replaceCode)
    res$insertBefore=c(res$insertBefore,res$Exp)
    res$insertAfter=c(res$insertAfter,releaseCode)
    res$Exp=as.symbol(varName)
    
    res
}


# convert a function to an expression
funcToExp <- function(f) {
    charExp = deparse(f)
    parsedExp = parse(text = charExp)[[1]]
    args = parsedExp[[2]]
    code = as.list(parsedExp[[3]])
    if (code[[1]] == "{") 
        code = code[-1]
    return(list(args = args, code = code))
}

# ================parser 2=================

#curExp=parse(text=paste0("for(i in a){a+b}"))[[1]]
extract_for_if_Var <- function(parsedExp) {
    code = c()
    for (i in seq_along(parsedExp)) {
        curExp = parsedExp[[i]]
        if (!is.call(curExp)) {
            code = c(code, curExp)
            next
        }
        if (curExp[[1]] == "for") {
          # Force substitution of the index variable
          index_var = curExp[[2]]
          loop_range= curExp[[3]]
          
          #If the loop range is not simply a : function, replace it
          if(!(is.call(loop_range)&&loop_range[[1]]==":")){
            index_newVar = GPUVar$getTmpVar()
            index_def_code = paste0(index_newVar, "=Scalar(precision=\"", 
                                    GPUVar$default_index_type, "\",constDef=TRUE)")
            
            index_release_code=parse(text=paste0("compiler.release(",index_newVar,")"))[[1]]
            if (is.symbol(loop_range)) {
              loop_rangeVar = deparse(loop_range)
              loop_range_def_Code = NULL
            } else {
              loop_rangeVar = GPUVar$getTmpVar()
              loop_range_def_Code = paste0(loop_rangeVar, "=", deparse(loop_range))
            }
            
            loop_rangeNew=paste0("1:length(",loop_rangeVar, ")")
            
            
            
            # assign the value to the looped variable
            index_var_code = paste0(deparse(index_var), "=", loop_rangeVar, 
                                    "[", index_newVar, "]")
            #index_var_release_code=parse(text=paste0("compiler.release(",deparse(index_var),")"))[[1]]
            
            
            index_def_code = parse(text = index_def_code)[[1]]
            if (!is.null(loop_range_def_Code)) 
              loop_range_def_Code = parse(text = loop_range_def_Code)[[1]]
            loop_rangeNew=parse(text = loop_rangeNew)[[1]]
            
            index_var_code = parse(text = index_var_code)[[1]]
            
            code = c(code, index_def_code, loop_range_def_Code)
            
            curExp[[2]] = as.symbol(index_newVar)
            curExp[[3]] = loop_rangeNew
            
            loopBody = curExp[[4]]
            loopBody_new = extract_for_if_Var(loopBody)
            
            loopBody_new = c(loopBody_new[1], index_var_code, loopBody_new[-1])
            curExp[[4]] = as.call(loopBody_new)
          }else{
            index_def_code = parse(text=paste0(index_var, "=Scalar(precision=\"", 
                                    GPUVar$default_index_type, "\",constDef=TRUE)"))[[1]]
            #index_release_code=parse(text=paste0("compiler.release(",index_var,")"))[[1]]
            #index_var_release_code=NULL
            index_release_code=NULL
          }
          code = c(code, index_def_code,curExp,index_release_code)
          next
        }
        # if (curExp[[1]] == "if") {
        #     condition = curExp[[2]]
        #     if (!is.symbol(condition)) {
        #         conditionVar = GPUVar$getTmpVar()
        #         conditionVar_def_code = paste0(conditionVar, "=", deparse(condition))
        #         
        #         conditionVar_def_code = parse(text = conditionVar_def_code)[[1]]
        #         
        #         code = c(code, conditionVar_def_code)
        #         curExp[[2]] = as.symbol(conditionVar)
        #         
        #     }
        #     code = c(code, curExp)
        #     next
        # }
        
        code = c(code, curExp)
    }
    return(code)
}


