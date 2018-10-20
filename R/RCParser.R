T_scale="scale"
T_matrix="matrix"
GPUVar=list()
#Arguments
GPUVar$gpu_tmp_var="gpuMagic_tmp"
#Per worker length
GPUVar$gpu_tmp_length_arg="gpu_tmp_length_arg"
GPUVar$gpu_matrix_size1="gpu_matrix_size1"
GPUVar$gpu_matrix_size2="gpu_matrix_size2"
GPUVar$gpu_return_variable="gpu_return_variable"
#Per worker size
GPUVar$gpu_return_size="gpu_return_size"
#The vector that is looped on
GPUVar$gpu_worker_data="gpu_worker_data"


#Deducted variable
GPUVar$gpu_tmp_matrix_offSize="gpu_matrix_offSize"
GPUVar$gpu_global_id="gpu_global_id"
GPUVar$gpu_tmp_length="gpu_tmp_length"
GPUVar$gpu_worker_offset="gpu_worker_offset"



RCcompilerLevel1<-function(profileMeta3){
  tmpInd=profileMeta3$tmpInd
  parsedExp=profileMeta3$Exp
  varInfo=profileMeta3$varInfo
  profile=varInfo$profile
  
  gpu_tmp_var=GPUVar$gpu_tmp_var
  gpu_tmp_length_arg=GPUVar$gpu_tmp_length_arg
  gpu_tmp_matrix_offSize=GPUVar$gpu_tmp_matrix_offSize
  gpu_matrix_size1=GPUVar$gpu_matrix_size1
  gpu_matrix_size2=GPUVar$gpu_matrix_size2
  gpu_result=GPUVar$gpu_result
  #Deducted variables
  gpu_global_id=GPUVar$gpu_global_id
  gpu_tmp_length=GPUVar$gpu_tmp_length
  gpu_worker_offset=GPUVar$gpu_worker_offset
  
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
                     gpu_tmp_matrix_offSize,"[",gpu_matrix_num,"]",");")
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      gpu_matrix_num=gpu_matrix_num+1
      next
    }
  }
  
  varInfo$profile=profile
  
  
  gpu_code=c(gpu_code,paste0(getTypeCXXStr(T_DEFAULT_float)," ",profileMeta3$workerData,";"))
  gpu_code=c(gpu_code,paste0(profileMeta3$workerData,"=",GPUVar$gpu_worker_data,"[",gpu_global_id,"];"))
  gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
             
  GPUExp1=profileMeta3
  GPUExp1$tmpInd=tmpInd
  GPUExp1$Exp=parsedExp
  GPUExp1$varInfo=varInfo
  GPUExp1$gpu_code=gpu_code
  
      
  return(GPUExp1)
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
      returnVar=curExp[[2]]
      returnInfo=getVarInfo(varInfo,returnVar)
      if(returnInfo$dataType==T_matrix){
        curCode=paste0("for(unsigned long gpu_return_i=0;gpu_return_i<",GPUVar$gpu_return_size,";gpu_return_i++){")
        curCode=c(curCode,paste0(GPUVar$gpu_return_variable,"[gpu_return_i]=",returnInfo$address,"[gpu_return_i+",GPUVar$gpu_global_id,"*",GPUVar$gpu_return_size,"];\n}"))
      }else{
        curCode=paste0(GPUVar$gpu_return_variable,"[",GPUVar$gpu_global_id,"]=",returnInfo$address,";")
      }
      gpu_code=c(gpu_code,curCode)
    }
  }
  return(gpu_code)
}

