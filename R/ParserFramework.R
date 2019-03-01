# codeMetaInfo=curMetaInfo
if (FALSE) {
    parserFunc = RLevel1_parserFunc
    checkFunc = RLevel1_checkFunc
    updateFunc = RLevel1_updateFunc
    codeMetaInfo = codeMetaInfo0
    level = c("top")
}
parserFrame <- function(parserFunc, checkFunc, updateFunc, codeMetaInfo) {
  res=parserFrame_hidden(parserFunc, checkFunc, updateFunc, codeMetaInfo)
  codeMetaInfo=res$codeMetaInfo
  codeMetaInfo$Exp=res$processedExp
  return(codeMetaInfo)
}

parserFrame_hidden<-function(parserFunc, checkFunc, updateFunc, codeMetaInfo, 
                      level = c("top")){
  codePack=list()
  codePack$codeMetaInfo = codeMetaInfo
  codePack$previousExp = codeMetaInfo$Exp
  codePack$processedExp = c()
  
  codePack$codeMetaInfo$Exp=NULL
  isTop=length(level)==1
  for (i in seq_along(codePack$previousExp)) {
    # if(i==3) stop()
    curExp = codePack$previousExp[[i]]
    if (curExp == "{" && length(curExp) == 1) {
      next
    }
    codePack$curExp=curExp
    codePack$i=i
    if(isTop){
      codePack$insertInMainBefore=NULL
      codePack$insertInMainAfter=NULL
    }
    codePack$insertAfter=NULL
    if (is.call(curExp)) {
      if (curExp[[1]] == "for") {
        loopIndExp=curExp[[3]]
        if (checkFunc(loopIndExp)) {
          curLevel = c(level, "for","condition")
          codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                                   curLevel,codePack,3)
        }
        curLevel = c(level, "for","body")
        codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                                 curLevel,codePack,4)
        codePack=finializeCodePack(codePack,isTop)
        next
      }
      
      
      if (curExp[[1]] == "if") {
        conditionExp = curExp[[2]]
        if (checkFunc(conditionExp)) {
          curLevel = c(level, "if","condition")
          codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                                   curLevel,codePack,2)
        }
        curLevel = c(level, "if",1,"body")
        codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                                 curLevel,codePack,3)
        if(length(curExp)>3){
          curLevel = c(level, "if",2,"body")
          codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                                   curLevel,codePack,4)
        }
        codePack=finializeCodePack(codePack,isTop)
        next
      }
    }
    # I don't know who will use double bracket But I add it for making sure
    # it works
    if (is.call(curExp) && curExp[[1]] == "{") {
      message("Unnecessary { has been found: ", deparse(curExp))
      
      curLevel = level
      codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                               curLevel,codePack,2)
      codePack$curExp=curExp[[-1]]
      codePack=finializeCodePack(codePack,isTop)
      next
    }
    
    if (checkFunc(curExp)) {
      curLevel = c(level, "singleCode")
      codePack=processCodePack(parserFunc, checkFunc, updateFunc,
                               curLevel,codePack,0)
      codePack=finializeCodePack(codePack,isTop)
      next
    }
  }
  return(codePack)
}


processCodePack<-function(parserFunc, checkFunc, updateFunc,
                curLevel,codePack,Exp_i){
  
  curExp=codePack$curExp
  if(Exp_i!=0){
    if(curLevel[length(curLevel)]=="body"){
      sub_Exp=toCodeChunk(curExp[[Exp_i]])
    }else{
      sub_Exp=curExp[[Exp_i]]
    }
  }else{
    sub_Exp=curExp
  }
  
  if(is.call(sub_Exp)&&sub_Exp[[1]]=="{"){
    res = ProcessCodeChunk(parserFunc,checkFunc, updateFunc, codePack$codeMetaInfo, 
                            curLevel,codePack$previousExp, codePack$processedExp, codePack$i, sub_Exp)
  }else{
    res = ProcessCodeSingle(parserFunc, updateFunc, codePack$codeMetaInfo, 
                            curLevel,codePack$previousExp, codePack$processedExp, codePack$i, sub_Exp)
  }
  
  codePack$codeMetaInfo = res$codeMetaInfo
  codePack$previousExp = res$previousExp
  codePack$processedExp = res$processedExp
  codePack$insertInMainBefore=c(codePack$insertInMainBefore,res$insertInMainBefore)
  codePack$insertInMainAfter=c(codePack$insertInMainAfter,res$insertInMainAfter)
  codePack$insertAfter=c(codePack$insertAfter,res$insertAfter)
  
  if(Exp_i!=0){
    codePack$curExp[[Exp_i]] = res$Exp
  }else{
    codePack$curExp = res$Exp
  }
  codePack
}

finializeCodePack<-function(codePack,isTop){
  if(isTop){
    codePack$processedExp=c(codePack$processedExp,codePack$insertInMainBefore,codePack$curExp,codePack$insertAfter,codePack$insertInMainAfter)
  }else{
    codePack$processedExp=c(codePack$processedExp,codePack$curExp,codePack$insertAfter)
  }
  codePack
}

# inside the for and if loop
ProcessCodeChunk <- function(parserFunc, checkFunc, updateFunc, codeMetaInfo, 
    curLevel, previousExp, processedExp, i, ExpChunk) {
    curMetaInfo = codeMetaInfo
    
    curMetaInfo$Exp = ExpChunk
    curMetaInfo$renameList = NULL
    res = parserFrame_hidden(parserFunc, checkFunc, updateFunc, curMetaInfo, curLevel)
    curMetaInfo=res$codeMetaInfo
    if ("renameList" %in% names(curMetaInfo)) {
      previousExp = renamevariable(previousExp, res$renameList, i)
      curMetaInfo$renameList = rbind(codeMetaInfo$renameList, curMetaInfo$renameList)
    }
    #type, level, codeMetaInfo, parsedExp, code, i, res
    res1 = updateFunc(type = "block", curLevel, curMetaInfo, res$previousExp, 
                      res$processedExp, i, res)
    if ("codeMetaInfo" %in% names(res1)) 
      codeMetaInfo = res1$codeMetaInfo
    if ("processedExp" %in% names(res1)) 
      ExpChunk=res1[["processedExp"]]
    else
      ExpChunk=res[["processedExp"]]
    ExpChunk = toCodeChunk(ExpChunk)
    list(codeMetaInfo = codeMetaInfo, previousExp = previousExp, processedExp = processedExp, 
        Exp = ExpChunk,insertInMainBefore=res$insertInMainBefore,insertInMainAfter=res$insertInMainAfter)
}
# For a single line code 
# ########parserFunc######## 
# parserFunc should at least return a list with Exp as the element, 
# the Exp is the current expression 
# Optional return value: 
# insertBefore: The expressions that will be added before the current expression 
# insertAfter: The expressions that will be added before the current expression 
# insertInMainBefore: The code inserted before the code chunk(if and for body)
# insertInMainAfter: The code inserted after the code chunk(if and for body)
# renameList: renaming a variable, the framework is responsible to 
# rename the variable in all the expressions next to the current one, the current one should be
# manually renamed. 
# #########updateFunc######## 
# updateFunc can be used to update anything in the codeMetaInfo, 
# parsedExp or code 
# Optional return value: 
# codeMetaInfo: The description object to describe the property of the code 
# parsedExp: The expression that is parsing, usually not change
# code: The parsed expression
ProcessCodeSingle <- function(parserFunc, updateFunc, codeMetaInfo, 
                              curLevel, previousExp, processedExp, i, Exp) {
    res = parserFunc(curLevel, codeMetaInfo, Exp)
    
    Exp = res$Exp
    processedExp = c(processedExp, res$insertBefore)
    
    res1 = updateFunc(type = "normal", curLevel, codeMetaInfo, previousExp, 
                      processedExp, i, res)
    if ("codeMetaInfo" %in% names(res1)) 
        codeMetaInfo = res1$codeMetaInfo
    if ("previousExp" %in% names(res1)) 
      previousExp = res1$previousExp
    if ("processedExp" %in% names(res1)) 
      processedExp = res1$processedExp
    if ("renameList" %in% names(res)) {
      previousExp = renamevariable(previousExp, res$renameList, i)
        codeMetaInfo$renameList = rbind(codeMetaInfo$renameList, res$renameList)
    }
    
    processedExp = c(processedExp)
    
    list(codeMetaInfo = codeMetaInfo, previousExp = previousExp, processedExp = processedExp, 
        Exp = Exp,insertInMainBefore=res$insertInMainBefore,insertInMainAfter=res$insertInMainAfter,insertAfter=res$insertAfter)
}
renamevariable <- function(parsedExp, renameList, i) {
    for (j in seq_len(nrow(renameList))) {
      parsedExp = renameVarInCode(parsedExp, 
        i, renameList[j, 1], renameList[j, 2])
    }
    parsedExp
}
renameVarInCode <- function(code, start, oldName, newName) {
    oldName = as.character(oldName)
    newName = as.symbol(newName)
    if (start <= length(code)) {
        for (i in start:length(code)) {
            renameList = list(newName)
            names(renameList) = oldName
            code[[i]] = do.call("substitute", list(code[[i]], renameList))
        }
    }
    return(code)
}

general_updateFunc <- function(codeMetaInfo, parsedExp, code) {
    result = list()
    result$codeMetaInfo = codeMetaInfo
    result$parsedExp = parsedExp
    result$code = code
    result
}


#Convert expressions to a code chunk
toCodeChunk <- function(Exp) {
  if (!(is.call(Exp) && Exp[[1]] == "{")) {
    Exp = as.call(c(as.symbol("{"), Exp))
  } else {
    Exp = as.call(Exp)
  }
  return(Exp)
}