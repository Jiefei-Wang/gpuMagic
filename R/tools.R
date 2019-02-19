# Remove the useless parenthesis, eg. ((a))
removeExpParenthesis <- function(Exp) {
  if (is.call(Exp) && Exp[[1]] == "(") 
    return(removeExpParenthesis(Exp[[2]]))
  return(Exp)
}

# Exp=quote(a[1,]) This function will return a list of the arguments of
# the [] function, the empty argments are expressed in character
# example: a[1] ==>i=1,drop=TRUE a[1,] ==>i=1,b='',drop=TRUE
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
#offset: if the result needs to replace an element of the expression, this is used to determine the offset of the index of the element
#insertBefore, insertAfter , insertInMainBefore, insertInMainAfter
combineInsertCode<-function(result,...,offset=0,autoOffset=TRUE){
  parms = list(...)
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

# combine the expression info from several expInfo
combineExpInfo <- function(result, ...,offset=0,autoOffset=TRUE) {
  if(is.language(result))stop("Incorrect old code")
  result=combineInsertCode(result, ...,offset=offset,autoOffset=autoOffset)
  parms = list(...)
  for (i in seq_along(parms)) {
    curInfo = parms[[i]]
    result$errorCheck = rbind(result$errorCheck, curInfo$errorCheck)
  }
  return(result)
}

