#######################Optimizer 1#############################
#Find which variables are in used
#Can handle redirect and subref
variableInUsed<-function(varInfo,vars){
  vars=vars[vars!=""]
  res=lapply(vars,findRootVar,varInfo=varInfo)
  varind=which(vapply(vars, hasVar.varInfo,x=varInfo,FUN.VALUE=logical(1)))
  unique(c(unlist(res),vars[varind]))
}
#Find the in used variables for the current expression
findRootVar<-function(varInfo,Exp){
  Exp=toExpression(Exp)
  vars=extractVars(Exp)
  root=c()
  for(i in vars){
    if(is.call(i)){
      root=c(root,findRootVar(varInfo,i))
      next
    }
    curvar=toCharacter(i)
    if(hasVar(varInfo,curvar)){
      curInfo=getVarInfo(varInfo,curvar)
      if(curInfo$redirect!="NA"){
        root=c(root,findRootVar(varInfo,curInfo$redirect))
      }else{
        if(curInfo$specialType=="ref"){
          root=c(root,findRootVar(varInfo,curInfo$specialContent))
        }else{
          if(!curInfo$shared){
            root=c(root,curInfo$var)
          }
        }
      }
    }
  }
  root
}
#######################Optimizer 2#############################
#Get the variable record and the promised record
getVarRecord<-function(previousCode){
  varRecord=c()
  promiseRecord=list()
  mainLineNum=0
  
  mainLineNum=which(previousCode=="//Main function delimiter")
  curMainNum=0
  nextMainNum=mainLineNum[1]
  mainNumInd=1
  for(i in seq_along(previousCode)){
    if(nextMainNum==i)next
    if(nextMainNum<i){
      mainNumInd=mainNumInd+1
      curMainNum=nextMainNum
      if(mainNumInd<=length(mainLineNum)){
        nextMainNum=mainLineNum[mainNumInd]
      }else{
        nextMainNum=length(previousCode)
      }
    }
    curCodeChunk=previousCode[[i]]
    codes=strsplit(curCodeChunk,"\n")[[1]]
    for(curCode in codes){
      #If it is a promise assign, skip it
      if(length(grep(GPUVar$promiseAssgin,curCode,fixed = TRUE))!=0){
        promiseRecord=extract_promise_assign_info(promiseRecord,curCode,curMainNum)
        next
      }
      if(length(grep(GPUVar$promiseDef,curCode,fixed = TRUE))!=0){
        varRecord=extract_promise_define_info(varRecord,curCode,curMainNum)
        next
      }
      #find the first main function delimiter
      if(curMainNum==0)
        next
      if(length(grep("//",curCode,fixed = TRUE))!=0){
        next
      }
      
      vars=extractVars(curCode)
      
      existingVarsInd=which(varRecord[,2]%in%vars)
      #Update the information for the existing variables
      for(j in existingVarsInd){
        varRecord[existingVarsInd,4]=curMainNum
      }
      nonExistingVarsInd=which(vars%in%varRecord[,2])
      if(length(nonExistingVarsInd)!=0)
        vars=vars[-which(vars%in%varRecord[,2])]
      
      
      match_info=extract_dim_var_info(vars)
      for(j in seq_along(match_info$var)){
        curVar=match_info$var[j]
        curPrecision=match_info$precision[j]
        curNote=match_info$note[j]
        varRecord=rbind(varRecord,c(curPrecision,curVar,curMainNum,curMainNum,curNote))
      }
    }
  }
  varRecord=as.data.frame(varRecord,stringsAsFactors=FALSE)
  colnames(varRecord)=c("precision","var","start","end","note")
  numericVar=c("start","end")
  for(i in numericVar)
    varRecord[,i]=as.numeric(varRecord[,i])
  list(var=varRecord,promiseRecord=promiseRecord)
}

#Get the dim variables info(name + precision)
extract_dim_var_info<-function(code){
  pattern=paste0(GPUVar$matrix_size_prefix,"([0-9A-Za-z_]+)_dim_([12])")
  #Find the target variable
  matched_var=unique(unlist(str_extract_all(code,pattern)))
  if(length(matched_var)==0)
    return(data.frame(var=character(0)))
  return(data.frame(note="dim",var=matched_var,precision=GPUVar$default_index_type,stringsAsFactors = FALSE))
}
#Get the promise info
extract_promise_assign_info<-function(promiseRecord,code,lineNum){
  promisePattern=paste0(GPUVar$promiseAssgin,"(.+?)","--","(.+)")
  matched_info=str_match(code,promisePattern)
  if(is.na(matched_info[1]))
    stop("fail to process promise assginment:" ,code)
  promiseVars=extractVars(matched_info[,2])
  promiseAssign=matched_info[,3]
  assignRelatedVars=extractVars(promiseAssign)
  assignRelatedVars=unique(assignRelatedVars[assignRelatedVars%in%promiseVars])
  assignRelatedVars=assignRelatedVars[!vapply(assignRelatedVars,isNumeric,logical(1))]
  if(length(assignRelatedVars)==0)assignRelatedVars=NULL
  for(promiseVar in promiseVars){
    key=paste0(promiseVar,"+",lineNum)
    promiseRecord$header=rbind(
      promiseRecord$header,
      data.frame(
        key=key,
        target=promiseVar,
        lineNum=lineNum,
        assignment=promiseAssign,
        stringsAsFactors = FALSE)
    )
    promiseRecord$relatedVars[[key]]=assignRelatedVars
  }
  return(promiseRecord)
}
extract_promise_define_info<-function(varRecord,curCode,curMainNum){
  pattern=paste0(GPUVar$promiseDef,"(.+?)","--","(.+)")
  matched_var=str_match(curCode,pattern)
  if(is.na(matched_var[1]))
    stop("fail to process promise define:" ,curCode)
  curNote="promiseDefine"
  varRecord=rbind(varRecord,
                  c(matched_var[2],matched_var[3],curMainNum,curMainNum,curNote))
  return(varRecord)
}


#Rule:
#1.If a promise assginment is found before the variable end,
#The promise assignment will be realized
#2.If the promise assignment is before the variable start
#the variable start will be set to the line number same as the promise define
#3.If the promise assignment involves other variables that in the variable table.
#Check the variable start line and make sure the variable starts before the promise define
computeVarRecordRange<-function(varRecord){
  varTbl=varRecord$var
  promiseRec=varRecord$promiseRecord
  promiseVarTbl=promiseRec$header
  if(is.null(promiseVarTbl)) return(varRecord)
  promiseVarTbl=cbind(promiseVarTbl,isRealized=FALSE)
  promiseRelatedVarTbl=promiseRec$relatedVars
  changed=TRUE
  while(changed){
    changed=FALSE
    for(i in seq_len(nrow(promiseVarTbl))){
      curPromise=promiseVarTbl[i,]
      if(curPromise$isRealized) next
      check_res=checkPromiseRealization(varTbl,promiseVarTbl,promiseRelatedVarTbl,i)
      varTbl=check_res$varTbl
      promiseVarTbl=check_res$promiseVarTbl
      changed=changed||check_res$changed
    }
  }
  list(varTbl=varTbl,promiseVarTbl=promiseVarTbl)
}
#Check if the vaiable table is valid for the current promise
checkPromiseRealization<-function(varTbl,promiseVarTbl,promiseRelatedVarTbl,i){
  curPromise=promiseVarTbl[i,]
  changed=FALSE
  promise_line=promiseVarTbl$lineNum
  curPromise_target=curPromise$target
  curInd=which(varTbl$var==curPromise_target)
  if(length(curInd)!=0){
    #Check the targeted promise assigned variable to make sure 
    # 1.it need the realization 
    # 2.the realization of the target var should be before the var start
    promise_check=checkVarTblLineNum(varTbl,var,promise_line,curInd)
    if(is.null(promise_check$end)){
      promiseVarTbl[i,]$isRealized=TRUE
    }
    if(!is.null(promise_check$start)){
      varTbl[curInd,]$start=promise_check$start
    }
    #Check the related vars and make sure they has been realized before the
    #promise assignment
    key=curPromise$key
    for(relatedVar in promiseRelatedVarTbl[[key]]){
      related_ind=which(varTbl$var==relatedVar)
      promise_check=checkVarTblLineNum(varTbl,relatedVar,promise_line,related_ind)
      if(!is.null(promise_check$end)){
        changed=TRUE
        varTbl[related_ind,]$end=promise_check$end
      }
      if(!is.null(promise_check$start)){
        varTbl[related_ind,]$start=promise_check$start
      }
    }
  }
  return(list(varTbl=varTbl,promiseVarTbl=promiseVarTbl,changed=changed))
}
#Check if the variable start and end cover the line number
checkVarTblLineNum<-function(varTbl,var,lineNum,ind=NULL){
  if(is.null(ind)){
    ind=which(varTbl$var==var)
  }
  curVarTbl=varTbl[ind,]
  start=NULL
  end=NULL
  if(curVarTbl$end<lineNum){
    end=lineNum
  }
  if(curVarTbl$start>lineNum){
    start=lineNum
  }
  return(list(start=start,end=end))
}

getInsertedCode<-function(varInfo,varTbl){
  if(is.null(varTbl))
    return(list())
  insertCode=list()
  #tmpname,varName
  temp_used_record=c()
  temp_used_precision=c()
  matrix_temporary_space=GPUVar$matrix_temporary_space
  for(i in seq_len(nrow(varTbl))){
    start_line=varTbl$start[i]
    #Remove the unused dim
    unused_ind=which(varTbl$end<start_line)
    unused_var=varTbl$var[unused_ind]
    temp_used_record[temp_used_record%in%unused_var]=NA
    
    #Assign the size variable a temporary dim variable
    curVar=varTbl$var[i]
    curPrecision=varTbl$precision[i]
    curNote=varTbl$note[i]
    #Check if there is enough temprary variables
    #If not, add more temporary variables
    #If it is enough, assign the temporary variables to the dim variables
    tmpVar_available_var=which(is.na(temp_used_record)&temp_used_precision==curPrecision)
    if(length(tmpVar_available_var)==0){
      temp_used_record=c(temp_used_record,curVar)
      temp_used_precision=c(temp_used_precision,curPrecision)
      temp_ind=length(temp_used_record)
      temp_Var_def=paste0(curPrecision," ",matrix_temporary_space,temp_ind,";")
    }else{
      temp_ind=tmpVar_available_var[1]
      temp_used_record[temp_ind]=curVar
      temp_Var_def=NULL
    }
    
    
    #Define the size macro
    temp_macro=paste0("#define ",curVar," ",matrix_temporary_space,temp_ind)
    #Find the size data
    var_data=NULL
    if(curNote=="dim"){
      pattern=paste0(GPUVar$matrix_size_prefix,"([0-9A-Za-z_]+)_dim_([12])")
      matchedData=str_match(curVar,pattern)
      targetVar=matchedData[2]
      targetdim=matchedData[3]
      if(has.key(targetVar,varInfo$matrixInd)){
        var_ind=varInfo$matrixInd[[targetVar]]
        var_info=varInfo[[var_ind[2]]][as.numeric(var_ind[1])+1,]
        var_data=paste0(curVar,"=",var_info[,paste0("size",targetdim,"_char")],";")
      }
    }
    start_line=as.character(start_line)
    insertCode[[start_line]]=c(insertCode[[start_line]],
                               temp_Var_def,
                               temp_macro,
                               var_data
    )
  }
  insertCode
}
realizePromiseAssign<-function(insertCode,promiseTbl){
  if(is.null(promiseTbl))
    return(insertCode)
  promiseTbl=promiseTbl[promiseTbl$isRealized,]
  for(i in seq_len(nrow(promiseTbl))){
    curPromise=promiseTbl[i,]
    lineNum=as.character(curPromise$lineNum)
    insertCode[[lineNum]]=c(insertCode[[lineNum]],
                            curPromise$assignment)
  }
  return(insertCode)
}
#=====================================Optimizer 3===========================
