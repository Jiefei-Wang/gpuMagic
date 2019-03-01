# Remove the useless parenthesis, eg. ((a))
removeExpParenthesis <- function(Exp) {
  if (is.call(Exp) && Exp[[1]] == "(") 
    return(removeExpParenthesis(Exp[[2]]))
  return(Exp)
}

# Exp=quote(a[1,]) This function will return a list of the arguments of
# the [] function, the empty argments are expressed in character
# example: 
# a[1] ==>i=1,drop=TRUE
# a[1,] ==>i=1,b='',drop=TRUE
# Exp=quote(a[])
matchBracketFunc <- function(Exp) {
  res = list(drop = TRUE)
  argName = names(Exp)
  argList = c("i", "j", "drop")
  
  if (is.null(argName)) {
    if (length(Exp) < 3) 
      stop("Unexpected expression:", deparse(Exp))
    for (i in 3:length(Exp)) {
      res[[argList[i - 2]]] = Exp[[i]]
    }
    for (i in seq_along(res)) {
      if (deparse(res[[i]]) == "") 
        res[[i]] = ""
    }
    return(res)
  }
  
  for (i in 3:length(argName)) {
    if (argName[i] != "") {
      res[[argName[i]]] = Exp[[i]]
    } else {
      for (k in seq_len(3)) {
        if (!(argList[k] %in% names(res))) {
          res[[argList[k]]] = Exp[[i]]
          break
        }
      }
    }
  }
  if (res[["drop"]] == "T") 
    res[["drop"]] = "TRUE"
  
  for (i in seq_along(res)) {
    if (deparse(res[[i]]) == "") 
      res[[i]] = ""
  }
  res
}

#combine multiple results
#result: a list with Exp as element
#Exp: the current processing expression
#infoPack: A list object. If the infoPack is not null, it will overwrite the ... parms
#offset: if the result needs to replace an element of the expression, this is used to determine the offset of the index of the element
#autoOffset: if true, the index starts from 2. If false, the index starts from 0.
#insertBefore, insertAfter , insertInMainBefore, insertInMainAfter
combineInsertCode<-function(result,...,infoPack=NULL,offset=0,autoOffset=TRUE){
  if(is.null(infoPack)){
    parms = list(...)
  }else{
    parms = infoPack
  }
  autoOffset=ifelse(autoOffset,1,-1)
  for (i in seq_along(parms)) {
    curInfo = parms[[i]]
    result$insertBefore = c(result$insertBefore, curInfo$insertBefore)
    result$insertAfter = c(result$insertAfter, curInfo$insertAfter)
    result$insertInMainBefore = c(result$insertInMainBefore, curInfo$insertInMainBefore)
    result$insertInMainAfter = c(result$insertInMainAfter, curInfo$insertInMainAfter)
    if(!is.null(curInfo$Exp)){
      result$Exp[[i + autoOffset+offset]] = curInfo$Exp
    }
  }
  return(result)
}



# Determine which type can preserve the information of the information
# in type1 and type2
typeInherit <- function(type1, type2) {
  if (!is.character(type1)) 
    type1 = as.character(type1)
  if (!is.character(type2)) 
    type2 = as.character(type2)
  
  group_float = getFloatingPointType()
  group_int = getIntegerType()
  
  target_size = max(getTypeSize(type1), getTypeSize(type2))
  if (type1 %in% group_float || type2 %in% group_float) {
    for (i in rev(seq_along(group_float))) {
      if (target_size == getTypeSize(group_float[i])) 
        return(group_float[i])
    }
  }
  if (type1 %in% group_int || type2 %in% group_int) {
    for (i in rev(seq_along(group_int))) {
      if (target_size == getTypeSize(group_int[i])) 
        return(group_int[i])
    }
  }
  for (i in rev(seq_along(group_float))) {
    if (target_size == getTypeSize(group_float[i])) 
      return(group_int[i])
  }
  stop("Unsupported variable type!")
}

typeTruncate <- function(type) {
  if(type%in%c("bool","char"))
    return("int")
  if(type%in%c("half"))
    return("float")
  return(type)
}



# Test if x is an NA value
# support character and expression.
isNA <- function(x,C=TRUE) {
  if (is.character(x)) {
    if(C){
      return(CSimplify(x) == "NA")
    }else{
      return(Simplify(x) == "NA")
    }
  }
  return(is.na(x))
}
# Test if an input is a number 
# x can be a character or an expression
isNumeric <- function(x) {
  if(is.numeric(x)) return(TRUE)
  if (!is.call(x) && length(x) > 1) 
    return(FALSE)
  
  xExp = NULL
  try({
    xExp = toExpression(x)
  }, silent = TRUE)
  if (is.null(xExp)) 
    return(FALSE)
  if (is.call(xExp)) {
    if(xExp[[1]]=="(") return(isNumeric(xExp[[2]]))
    return(FALSE)
  }
  res = is.numeric(xExp)
  return(res)
}

#Test if a value is an integer
is.wholenumber=function(x, tol = .Machine$double.eps^0.5) {
  if(is.character(x)) x=as.numeric(x)
  abs(x - round(x)) < tol
  
}

is.preservedFunc <- function(func) {
  func = as.character(func)
  length(grep(GPUVar$preservedFuncPrefix, func, fixed = TRUE)) != 0
}



toCharacter <- function(x) {
  if (is.language(x)) {
    var_char = deparse(x)
  } else {
    if (is.character(x)) 
      var_char = x else {
        var_char = as.character(x)
      }
  }
  var_char
}
# Convert an non-expression to the expression and return both
# expression and characters
toExpression <- function(var) {
  if (is.language(var)) {
    var_char = deparse(var)
  } else {
    if (is.character(var)) 
      var_char = var else {
        var_char = as.character(var)
      }
    var = parse(text = var_char)[[1]]
  }
  return(var)
}



# This function simplify the R code and make it ready to put in the
# varInfo table
Simplify2 <- function(Exp,parentheses=TRUE,enhance=TRUE) {
  res = Simplify(Exp)
  if (isNumeric(res)) 
    return(res)
  if(enhance){
    res=parse(text=res)[[1]]
    res=deparse(Simplify_plus(res))
  }
  if(parentheses){
    return(paste0("(", res, ")"))
  }else{
    return(res)
  }
}

hasVar<-function(x,...){
  UseMethod("hasVar")
}



#' @return A vector of variables
#' @rdname internalFunctions
#' @export
extractVars <- function(x) UseMethod("extractVars")
#' @method extractVars default
#' @rdname internalFunctions
#' @export
extractVars.default <- function(x) {
  if(is.language(x)) 
    return(extractVars.expression(x))
  if(is.numeric(x))
    return(NULL)
  matchRes = gregexpr("[a-zA-Z0-9_]+", x)[[1]]
  vars = vapply(
    seq_along(matchRes), 
    function(i, x, start, len) 
      substr(x, start[i], start[i] + len[i] - 1), "",
    x, matchRes, attr(matchRes, "match.length")
  )
  vars
}

#' @method extractVars expression
#' @rdname internalFunctions
#' @export
extractVars.expression <- function(x) {
  if(!is.call(x)){
    return(deparse(x))
  }
  if(isNumeric(x)||deparse(x)=="")
    return(NULL)
  res=c()
  for(i in seq_len(length(x)-1)+1){
    if(!is.call(x[[i]])&&deparse(x[[i]])=="")next
    res=c(res,extractVars(x[[i]]))
  }
  return(res)
}
