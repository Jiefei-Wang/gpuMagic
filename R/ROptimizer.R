#add variable declare and release code in the expression
ROptimizer1 <- function(profileMeta2) {
  previousExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  #Find the start and end line number of the in used variables
  #a vector of 2: start,end
  varUsedInfo=list()
  for(i in seq_along(previousExp)){
    curExp=previousExp[[i]]
    vars=unique(extractVars(curExp))
    varsInUsed=variableInUsed(varInfo,vars)
    for(curVar in varsInUsed){
      if(is.null(varUsedInfo[[curVar]])){
        varUsedInfo[[curVar]]=c(i,i)
      }else{
        varUsedInfo[[curVar]][2]=i
      }
    }
  }
  #Remove the global variable
  varUsedInfo[[GPUVar$gpu_global_id]]=NULL
  varUsedInfoTbl=do.call("rbind", varUsedInfo)
  
  vars=row.names(varUsedInfoTbl)
  #Add compiler.define and compiler.release information in the code
  Exp=c()
  for(i in rev(seq_along(previousExp))){
    curReleasedVarInd=which(varUsedInfoTbl[,2]==i)
    curNewVarInd=which(varUsedInfoTbl[,1]==i)
    curReleasedVar=names(curReleasedVarInd)
    curNewVar=names(curNewVarInd)
    if(length(curNewVar)>0){
      defCode=paste0("compiler.define(",paste0(curNewVar,collapse = ","),")")
      defCode=parse(text=defCode)[[1]]
    }else{
      defCode=NULL
    }
    if(length(curReleasedVar)>0){
      releaseCode=paste0("compiler.release(",paste0(curReleasedVar,collapse = ","),")")
      releaseCode=parse(text=releaseCode)[[1]]
    }else{
      releaseCode=NULL
    }
    Exp=c(Exp,
          releaseCode,
          previousExp[[i]],
          defCode
    )
  }
  optMeta1_0=profileMeta2
  optMeta1_0$Exp=rev(Exp)
  optMeta1_1=RProfile1(optMeta1_0)
  optMeta1_2=RProfile2(optMeta1_1)
  optMeta1_2
}

#Redirect the dim variable if the other variable has the same value
ROptimizer2<-function(optMeta1){
  varInfo=optMeta1$varInfo
  dimRecord=data.frame(dimVar=character(0),value=character(0),stringsAsFactors = FALSE)
  for(curVar in extractVars(varInfo)){
    curInfo=getVarInfo(varInfo,curVar)
    if(!curInfo$dynSize1&&!is.na(curInfo$size1)&&!isNumeric(curInfo$size1)){
      dimVar=getSizeVar(curVar,1)
      dimRecord=rbind(dimRecord,c(dimVar,curInfo$size1),stringsAsFactors = FALSE)
    }
    if(!curInfo$dynSize2&&!is.na(curInfo$size2)&&!isNumeric(curInfo$size2)){
      dimVar=getSizeVar(curVar,2)
      dimRecord=rbind(dimRecord,c(dimVar,curInfo$size2),stringsAsFactors = FALSE)
    }
  }
  colnames(dimRecord)=c("dimVar","value")
  dimRecord$value=vapply(dimRecord$value,Simplify,FUN.VALUE = character(1))
  dimRecord_noDuplicate=dimRecord[!duplicated(dimRecord[,c('value')]),]
  dimMap=list()
  for(i in seq_len(nrow(dimRecord))){
    curRecord=dimRecord[i,]
    matched_ind=which(dimRecord_noDuplicate$value==curRecord$value)
    if(length(matched_ind)!=0){
      dimMap[[curRecord$dimVar]]=dimRecord_noDuplicate$dimVar[matched_ind]
    }
  }
  optMeta1$varInfo$dimMap=dimMap
  optMeta1
}




#Process promise define and promise assign
ROptimizer3 <- function(GPUExp2) {
  previousCode=GPUExp2$gpu_code
  #Find the start and end line number of the var dimension
  varRecord=getVarRecord(previousCode)
  varRecord=computeVarRecordRange(varRecord)
  
  insertCode=getInsertedCode(GPUExp2$varInfo,varRecord$var)
  insertCode=realizePromiseAssign(insertCode,varRecord$promiseVarTbl)
  code=previousCode
  if(!is.null(insertCode)){
    for(i in rev(as.numeric(names(insertCode)))){
      pre_len=i-1
      post_len=length(code)-i
      code=c(code[seq_len(pre_len)],insertCode[[as.character(i)]],code[seq_len(post_len)+i])
    }
  }
  code=code[-which(code=="//Main function delimiter")]
  
  
  GPUExp3=GPUExp2
  GPUExp3$gpu_code=code
  GPUExp3
}


