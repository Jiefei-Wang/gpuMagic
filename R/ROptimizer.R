ROptimizer1 <- function(profileMeta2) {
  previousExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  #Find the start and end line number of the in used variables
  #a vector of 2: start,end
  varUsedInfo=list()
  for(i in seq_along(previousExp)){
    curExp=previousExp[[i]]
    vars=unique(extractVars(curExp))
    varsInUsed=unique(variableInUsed(varInfo,vars))
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


ROptimizer2 <- function(GPUExp2) {
  previousCode=GPUExp2$gpu_code
  #Find the start and end line number of the var dimension
  varRecord=getVarDimRecord(previousCode)
  
  insertCode=getDimCode(GPUExp2$varInfo,varRecord)
  code=previousCode
  for(i in rev(as.numeric(names(insertCode)))){
    pre_len=i-1
    post_len=length(code)-i
    code=c(code[seq_len(pre_len)],insertCode[[as.character(i)]],code[seq_len(post_len)+i])
  }
  code=code[-which(code=="//Main function delimiter")]
  #realize the promise assign
  code=realizePromiseAssign(code,varRecord)
  
  GPUExp3=GPUExp2
  GPUExp3$gpu_code=code
  GPUExp3
}


getVarDimRecord<-function(previousCode){
  varRecord=c()
  mainLineNum=0
  pattern=paste0(GPUVar$matrix_size_prefix,"(.+?)_dim_([12])")
  for(i in seq_along(previousCode)){
    curCode=previousCode[[i]]
    if(curCode=="//Main function delimiter"){
      mainLineNum=i
      next
    }
    #If it is a promise assign, skip it
    if(length(grep(GPUVar$promiseAssgin,curCode,fixed = TRUE))!=0)
      next
    #find the first main function delimiter
    if(mainLineNum==0)
      next
    #extract the variable dimension variables
    dim_vars=unique(str_extract_all(curCode,pattern)[[1]])
    #Find the target variable
    vars=gsub(pattern,"\\1",dim_vars)
    vars_dim_ind=gsub(pattern,"\\2",dim_vars)
    for(j in seq_along(vars)){
      var=vars[j]
      var_dim_ind=vars_dim_ind[j]
      record_Ind=which(varRecord[,1]==var&varRecord[,2]==var_dim_ind)
      if(length(record_Ind)==1){
        varRecord[record_Ind,4]=i
      }else{
        varRecord=rbind(varRecord,c(var,var_dim_ind,i,i,mainLineNum))
      }
    }
  }
  varRecord=as.data.frame(varRecord,stringsAsFactors=FALSE)
  colnames(varRecord)=c("var","dim","start","end","mainLineNum")
  varRecord
}

getDimCode<-function(varInfo,varRecord){
  insertCode=list()
  #tmpname,varName
  temp_dim_record=c()
  ind=unique(sort(c(varRecord$start)))
  matrix_temporary_size=GPUVar$matrix_temporary_size
  for(i in seq_len(nrow(varRecord))){
    start_line=varRecord$start[i]
    #Remove the unused dim
    unused_ind=which(varRecord$end<start_line)
    unused_var=varRecord$var[unused_ind]
    unused_var_dim=varRecord$dim[unused_ind]
    var_dim=getSizeVar(unused_var,unused_var_dim)
    temp_dim_record[temp_dim_record%in%var_dim]=NA
    
    #Assign the size variable a temporary dim variable
    mainLineInd=varRecord$mainLineNum[i]
    curVar=varRecord$var[i]
    curVar_dim=varRecord$dim[i]
    curVar_dim_char=getSizeVar(curVar,curVar_dim)
    #Check if there is enough temprary variables
    #If not, add more temporary variables
    #If it is enough, assign the temporary variables to the dim variables
    temp_Var_def=NULL
    tmpVar_available_size=sum(is.na(temp_dim_record))
    if(tmpVar_available_size==0){
      temp_dim_record=c(temp_dim_record,NA)
      temp_Var_def=paste0(GPUVar$default_index_type," ",matrix_temporary_size,length(temp_dim_record),";")
    }
    tmpVar_available_ind=which(is.na(temp_dim_record))
    tmp_ind=tmpVar_available_ind[1]
    temp_dim_record[tmp_ind]=curVar_dim_char
    #Define the size macro
    dim_macro=paste0("#define ",curVar_dim_char," ",matrix_temporary_size,tmp_ind)
    #Find the size data
    dim_data=NULL
    if(has.key(curVar,varInfo$matrixInd)){
      curInfo=getVarInfo(varInfo,curVar)
      if(curInfo$location=="global"&&curInfo$shared){
        size_ad=paste0(GPUVar$gs_size,curVar_dim)
      }
      if(curInfo$location=="global"&&!curInfo$shared){
        size_ad=paste0(GPUVar$gp_size,curVar_dim)
      }
      if(curInfo$location=="local"&&curInfo$shared){
        size_ad=paste0(GPUVar$ls_size,curVar_dim)
      }
      if(curInfo$location=="local"&&!curInfo$shared){
        size_ad=paste0(GPUVar$lp_size,curVar_dim)
      }
      dim_data=paste0(curVar_dim_char,"=",size_ad,"(",varInfo$matrixInd[[curVar]],");")
    }
    
    insertCode[[mainLineInd]]=c(insertCode[[mainLineInd]],
                                temp_Var_def,
                                dim_macro,
                                dim_data
    )
  }
  insertCode
}
realizePromiseAssign<-function(code,varRecord){
  vars_dim=getSizeVar(varRecord$var,varRecord$dim)
  vars_dim_group=paste0(vars_dim,collapse = "|")
  vars_dim_header=GPUVar$promiseAssgin
  vars_dim_pattern=paste0(vars_dim_header,"(",vars_dim_group,")")
  remove_ind=NULL
  for(i in seq_along(code)){
    curCode=code[i]
    if(length(grep(vars_dim_header,curCode))>0){
      if(length(grep(vars_dim_pattern,curCode))>0){
        code[i]=substr(curCode,nchar(vars_dim_header)+1,nchar(curCode))
      }else{
        remove_ind=c(remove_ind,i)
      }
    }
  }
  if(!is.null(remove_ind))
    code=code[-remove_ind]
  return(code)
}
