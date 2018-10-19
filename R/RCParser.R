T_scale="scale"
T_matrix="matrix"

RCcompilerLevel1<-function(profileMeta2){
  tmpInd=profileMeta2$tmpInd
  parsedExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  profile=varInfo$profile
  GPUVar=list()
 
  #Define some gpu variable
  #Arguments
  gpu_tmp_var="gpuMagic_tmp"
  gpu_tmp_length_arg="gpu_tmp_length_arg"
  gpu_matrix_size1="gpu_matrix_size1"
  gpu_matrix_size2="gpu_matrix_size2"
  gpu_matrix_tmp_offSize="gpu_matrix_offSize"
  #Deducted variables
  gpu_global_id="gpu_global_id"
  gpu_tmp_length="gpu_tmp_length"
  gpu_worker_offset="gpu_worker_offset"
  
  
  
  GPUVar$gpu_tmp_var=gpu_tmp_var
  GPUVar$gpu_tmp_length_arg=gpu_tmp_length_arg
  GPUVar$gpu_matrix_size1=gpu_matrix_size1
  GPUVar$gpu_matrix_size2=gpu_matrix_size2
  GPUVar$gpu_matrix_tmp_offSize=gpu_matrix_tmp_offSize
  # varInfo$gpu_global_id=gpu_global_id
  # varInfo$gpu_tmp_length=gpu_tmp_length
  varInfo$GPUVar=GPUVar
  
  gpu_code=c(
    paste0("unsigned long ",gpu_global_id,"=get_global_id(0);"),
    paste0("unsigned long ", gpu_tmp_length,"=*",gpu_tmp_length_arg,";"),
    paste0("unsigned long ", gpu_worker_offset,"=",gpu_global_id,"*",gpu_tmp_length,";")
  )
  size_list=c()
  gpu_matrix_num=0
  
  for(i in 1:nrow(profile)){
    curVar=profile[i,]
    if(curVar$require=="Y"){
      curVar$address=curVar$var
      curVar$dataType=T_matrix
      next
    }
    if(curVar$dataType==T_scale){
      CXXtype=getTypeCXXStr(as.numeric(curVar$precisionType))
      #curVar$compileData=="Y"
      if(FALSE){
        value=eval(parse(text=curVar$value))
        curCode=paste(CXXtype,curVar$var,"=",value,";",sep = " ")
      }else{
        curCode=paste(CXXtype,curVar$var,";",sep = " ")
      }
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      next
    }
    
    if(curVar$dataType==T_matrix){
      dataType=as.numeric(curVar$precisionType)
      CXXtype=getTypeCXXStr(dataType)
      size1=curVar$size1
      size2=curVar$size2
      curCode=paste0(CXXtype,"* ",curVar$var,"=",
                     "(",CXXtype,"*)(",
                     gpu_tmp_var,"+",
                     gpu_worker_offset,"+",
                     gpu_matrix_tmp_offSize,"[",gpu_matrix_num,"]",");")
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      gpu_matrix_num=gpu_matrix_num+1
      next
    }
  }
  
  varInfo$profile=profile
  
  gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
      
  return(list(tmpInd=tmpInd,code=parsedExp,varInfo=varInfo,gpu_code=gpu_code))
}


RCTranslation<-function(varInfo,parsedExp){
  gpu_code=c()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(curExp=="{")
      next
    if(switch(deparse(curExp[[1]]),"="=T,"=="=T,F)){
      curCode=C_call_assign(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
    }
    if(curExp[[1]]=="for"){
      curCode=RCTranslation(varInfo,curExp[[4]])
      loopInd=curExp[[2]]
      loopRange=curExp[[3]]
      curVar=getVarInfo(varInfo,loopInd)
      dataType=as.numeric(curVar$precisionType)
      CXXtype=getTypeCXXStr(dataType)
      loopFunc=paste0("for(",CXXtype," ",loopInd,"=",loopRange[[2]],";",
                      loopInd,"<=",loopRange[[3]],";",
                      loopInd,"++){")
      gpu_code=c(gpu_code,loopFunc,curCode,"}")
    }
    if(curExp[[1]]=="if"){
      curCode=RCTranslation(varInfo,curExp[[3]])
      ifFunc=paste0("if(",deparse(curExp[[2]]),"){")
      gpu_code=c(gpu_code,ifFunc,curCode,"}")
    }
    if(curExp[[1]]=="return"){
      
      gpu_code=c(gpu_code,deparse(curExp),"return;")
    }
  }
  return(gpu_code)
}

