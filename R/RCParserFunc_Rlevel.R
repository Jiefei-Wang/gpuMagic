# C_element_getCExp(varInfo,quote((At+A)[ind,]),c(3,6)-1,opt=NULL)

R_expression_sub <- function(varInfo, Exp, sub, opt = NULL, extCode = NULL) {
    # fill the vector with the first element to make the length of it
    # consistant with the length of Exp
    
    curInfo = getVarInfo(varInfo, Exp)
    if (curInfo$isRef) {
      refExp = parse(text = curInfo$specialContent)[[1]]
      if (curInfo$transpose) {
        if(length(sub)==2){
          tmp=sub[1]
          sub[1]=sub[2]
          sub[2]=tmp
        }
      }
      res = C_element_getCExp(varInfo, refExp, sub,  opt = opt, extCode = extCode)
      return(res)
    }
    
    
    if (length(sub) == 1) {
        res = R_oneIndex_exp_sub(varInfo, Exp, sub, opt=opt, extCode=extCode)
    } else {
        res = R_getVarSub(varInfo, Exp, sub[1], sub[2], opt, extCode)
    }
    
      #code hoisting optimization
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
R_oneIndex_exp_sub <- function(varInfo, Exp, k, opt = NULL, extCode = NULL) {
    
    # If the expression is a variable
    curInfo = getVarInfo(varInfo, Exp)
    rowNum = R_nrow(varInfo,Exp)
    colNum=R_ncol(varInfo,Exp)
    res_index=one_to_two_index(k,extCode = extCode,rowNum=rowNum,colNum=colNum)
    res = R_getVarSub(varInfo, Exp, i = res_index$i, j = res_index$j, opt = opt, extCode = res_index$extCode)
    return(res)
}

#Return: i, j, extCode
one_to_two_index<-function(k,extCode,rowNum,colNum){
  if(rowNum=="1"){
    return(list(i=0,j=k,extCode=extCode))
  }
  if(colNum=="1"){
    return(list(i=k,j="0",extCode=extCode))
  }
  index_type=GPUVar$default_index_type
  col_ind_var = GPUVar$getTmpVar()
  col_ind_value = CSimplify(paste0("(", index_type, ")((", 
                                   k, ")/(", rowNum, "))"))
  # if the temporary variable is a constant, it will be plug into the
  # code Otherwise, create a temporary variable for it
  if (isNumeric(col_ind_value) || length(grep("/", col_ind_value, fixed = TRUE)) == 
      0) {
    col_ind_var = col_ind_value
  } else {
    res = getVarFromExtCode(extCode, index_type, col_ind_value)
    col_ind_var = res$var
    extCode = res$extCode
  }
  
  i = CSimplify(paste0(k, "-", rowNum, "*(", col_ind_var,")"))
  j = CSimplify(col_ind_var)
  
  return(list(i=i,j=j,extCode=extCode))
}
# Get an element from the matrix(eg. A[i,j]), the transpose will be
# taken into account 
# i,j are either a number or a variable in C code , they are 0-based index
# If opt=FALSE, 
# If opt=TRUE, a list will be returned. 
R_getVarSub <- function(varInfo, var, i, j = 1, opt = NULL, extCode = NULL) {
    var = toCharacter(var)
    
    if (var == "") 
        stop("something is wrong..")
    # return(list(value=toCharacter(i),extCode=extCode))
    
    curInfo = getVarInfo(varInfo, var)
    address = curInfo$address
    transpose = curInfo$transpose
    isPointer=curInfo$isPointer
    storageMode=curInfo$storageMode
    
    #If the data is not a pointer, it is a scalar, then directly return the address
    if(is.na(isPointer)) stop("Unable to determine the type of the variable,",
                              " this is a bug in the package, please contact the author.")
    
    
    
    if(!isPointer) {
      if(!curInfo$isSeq)
        return(list(value =address,extCode=extCode))
    }else{
      #If the data is a pointer, but the size is 1, then return the first element in the address
      if((curInfo$size1=="1"&&curInfo$size2=="1")||curInfo$dataType==T_scale){
        if(!curInfo$isSeq){
          return(list(value =paste0(address,"[0]"),extCode=extCode))
        }else{
          seqInfo = getSeqAddress(varInfo, var)
          return(list(value =seqInfo$from,extCode=extCode))
        }
      }
    }
    
    # compute the matrix offset
    size1 = R_getVarSize(varInfo, var,C_symbol = FALSE,ind=1,processTranspose = FALSE)
    
    if (transpose) {
        tmp = i
        i = j
        j = tmp
    }
    
    rowOffset = i
    colOffset = paste0("(", j, ")*(", size1,")")
    
    if (curInfo$isSeq) {
      seqInfo = getSeqAddress(varInfo, var)
      res = list(extCode=extCode)
      res$value = CSimplify(paste0(seqInfo$from, "+", "(", rowOffset, 
                                   ")*", seqInfo$by))
      return(res)
    }
    
    
    element_dist=switch(storageMode,"single"=1,"mixed"=GPUVar$element_dist)
    
    offset = CSimplify(paste0("(",rowOffset, "+", colOffset,")*",element_dist))
    if (!is.null(opt)&&
        as.logical(gpuMagic.getOptions("hoist.optimization"))) {
        res = hoistOpt(extCode, offset)
        offset = res$value
        extCode = res$extCode
    }
    c_sub = R_C_Sub(address, offset)
    
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

# R_nrow(varInfo,quote(A[ind1,]+B*ind))
# Get the number of rows for a matrix in C format
R_nrow <- function(varInfo, var,C_symbol=FALSE) {
  R_getVarSize(varInfo, var,C_symbol, 1,processTranspose=TRUE)
}
# Get the number of rows for a matrix in C format
R_ncol <- function(varInfo, var,C_symbol=FALSE) {
  R_getVarSize(varInfo, var,C_symbol, 2,processTranspose=TRUE)
}
R_length <- function(varInfo, var,C_symbol=FALSE) {
    return(CSimplify(paste0(R_nrow(varInfo, var,C_symbol), "*", R_ncol(varInfo, 
        var,C_symbol))))
}


R_getVarSize <- function(varInfo, var,C_symbol, ind,processTranspose=TRUE) {
    var_char = toCharacter(var)
    Exp = toExpression(var)
    if (isNumeric(var_char)) 
      return(1)
    #If the var is a variable name
    if (!is.call(Exp)){
      curInfo = getVarInfo(varInfo, Exp)
      
      if(curInfo$transpose&&processTranspose){
        ind=3-ind
      }
      if (curInfo$isRef) {
        Exp = parse(text = curInfo$specialContent)[[1]]
        res=R_getVarSize(varInfo, Exp,C_symbol, ind)
        return(res)
      }
      if (curInfo$isSeq) {
        res=ifelse(ind==1,getSizeVar(var_char,1),1)
        return(res)
      }
      if (curInfo$dataType == T_scale) 
        return(1)
      
      if(!C_symbol){
        size=switch(ind,curInfo$size1,curInfo$size2)
        if(isNumeric(size))
          return(as.numeric(size))
        size_var=getSizeVar(var_char,ind)
        if(!is.null(varInfo$dimMap[[size_var]]))
          return(varInfo$dimMap[[size_var]])
      }
      return(getSizeVar(var_char,ind))
    }
    #If the Expresison is a function, search for the available function
    func=Exp[[1]]
    if(!is.null(.sizeFuncs[[func]])){
      res=.sizeFuncs[[func]](varInfo, var,C_symbol, ind)
      return(res)
    }
    stop("Cannot determine the size of the expression: ",deparse(Exp))
}

combineSize<-function(sizeList){
  if(length(sizeList)==0) stop("Empty size list")
  sizeList_notOne=unique(sizeList[sizeList!="1"])
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
  #All variables are not number
  res=paste0("max(",sizeList_notOne[1],",",sizeList_notOne[2],")")
  for(i in seq_len(length(sizeList_notOne)-2)+2){
    res=paste0("max(",sizeList_notOne[i],",",res,")")
  }
  return(res)
}

R_general_size<-function(varInfo, Exp,C_symbol, ind){
  #If the function is an element operation
  #If the size are all 1, return 1
  #If one matrix has non-1 size, return the dimension of that matrix
  #If multiple matrix has non-1 size, return the maximum among them
  parms_size=c()
  nparms=length(Exp)-1
  for(i in seq_len(nparms)+1){
    parms_size=c(parms_size,R_getVarSize(varInfo, Exp[[i]],C_symbol, ind))
  }
  
  res=combineSize(parms_size)
  res
}

R_size_returnOne<-function(varInfo, Exp,C_symbol, ind){
  return(1)
}

R_subset_size<-function(varInfo, Exp,C_symbol, ind){
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
  stop("Incorrect size index")
}


R_sweep_size<-function(varInfo, Exp,C_symbol, ind){
  args=matchFunArg(sweep,Exp)
  target=args$x
  return(R_getVarSize(varInfo,target,C_symbol=C_symbol,ind=ind))
}




