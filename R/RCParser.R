#' @include pkgFunc.R

RCcompilerLevel1<-function(profileMeta3){
  tmpMeta=profileMeta3$tmpMeta
  parsedExp=profileMeta3$Exp
  varInfo=profileMeta3$varInfo
  profile=varInfo$profile
  
  #Preserved variables
  #Global worker private data
  gpu_worker_data=GPUVar$gpu_worker_data
  #Per worker length
  gpu_worker_data_size=GPUVar$gpu_worker_data_size
  gpu_worker_size1=GPUVar$gpu_worker_size1
  gpu_worker_size2=GPUVar$gpu_worker_size2
  #Per worker offsize
  gpu_worker_matrix_offSize=GPUVar$gpu_worker_matrix_offSize
  
  #worker shared data, located in global memory
  gpu_global_shared_data=GPUVar$gpu_global_shared_data
  gpu_global_shared_size1=GPUVar$gpu_global_shared_size1
  gpu_global_shared_size2=GPUVar$gpu_global_shared_size2
  gpu_global_shared_offSize=GPUVar$gpu_global_shared_offSize
  
  #worker private data, located in private memory
  gpu_worker_private_data=GPUVar$gpu_worker_private_data
  gpu_worker_private_size1=GPUVar$gpu_worker_private_size1
  gpu_worker_private_size2=GPUVar$gpu_worker_private_size2
  gpu_worker_private_offSize=GPUVar$gpu_worker_private_offSize
  
  
  
  #Deducted variable
  gpu_global_id=GPUVar$gpu_global_id
  gpu_worker_offset=GPUVar$gpu_worker_offset
  
  gpu_code=c(
    paste0("unsigned long ",gpu_global_id,"=get_global_id(0);"),
    paste0("unsigned long ", gpu_worker_data_size,"=*",gpu_worker_data_size,"_arg;"),
    paste0("unsigned long ", gpu_worker_offset,"=",gpu_global_id,"*",gpu_worker_data_size,";")
  )
  gpu_matrix_num=-1
  
  for(i in 1:nrow(profile)){
    curVar=profile[i,]
    if(curVar$dataType==T_matrix)
      gpu_matrix_num=gpu_matrix_num+1
    if(curVar$constant=="Y"){
      next
    }
    if(curVar$require=="Y"){
      curVar$address=curVar$var
      curVar$dataType=T_matrix
      next
    }
    if(curVar$initialization=="N"){
      profile[i,]$address=curVar$var
      next
    }
    if(curVar$dataType==T_scale){
      CXXtype=getTypeCXXStr(as.numeric(curVar$precisionType))
      
      curCode=paste0(CXXtype," ",curVar$var,";")
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      next
    }
    
    if(curVar$dataType==T_matrix){
      dataType=as.numeric(curVar$precisionType)
      CXXtype=getTypeCXXStr(dataType)
      size1=curVar$size1
      size2=curVar$size2
      ##########This need to be optimized
      curCode=switch(curVar$location,
             global=paste0("__global ",CXXtype,"* ",curVar$var,"=",
                           "(__global ",CXXtype,"*)(",
                           gpu_tmp_var,"+",
                           gpu_worker_offset,"+",
                           gpu_tmp_matrix_offSize,"[",gpu_matrix_num,"]",");"),
             private={
               preserved_code=paste0(GPUVar$private_mem,curVar$var)
               paste0("__private ",CXXtype," ",curVar$var,"[",preserved_code,"];")}
               )
      
      gpu_code=c(gpu_code,curCode)
      profile[i,]$address=curVar$var
      #gpu_matrix_num=gpu_matrix_num+1
      next
    }
  }
  
  varInfo$profile=profile
  
  
  #gpu_code=c(gpu_code,paste0(getTypeCXXStr(T_DEFAULT_float)," ",profileMeta3$workerData,";"))
  #gpu_code=c(gpu_code,paste0(profileMeta3$workerData,"=",GPUVar$gpu_worker_data,"[",gpu_global_id,"];"))
  gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
             
  GPUExp1=profileMeta3
  GPUExp1$tmpMeta=tmpMeta
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
      next
    }
    if(curExp[[1]]=="return"){
      returnVar=curExp[[2]]
      returnInfo=getVarInfo(varInfo,returnVar)
      if(returnInfo$dataType==T_matrix){
        curCode=paste0("for(unsigned long gpu_return_i=0;gpu_return_i<*",GPUVar$gpu_return_size,";gpu_return_i++){\n")
        curCode=c(curCode,paste0(GPUVar$gpu_return_variable,"[gpu_return_i+",GPUVar$gpu_global_id,"*(*",GPUVar$gpu_return_size,")]=",returnInfo$address,"[gpu_return_i];\n}\n"))
      }else{
        curCode=paste0(GPUVar$gpu_return_variable,"[",GPUVar$gpu_global_id,"]=",returnInfo$address,";\n")
      }
      gpu_code=c(gpu_code,curCode)
      next
    }
    
    if(curExp[[1]]=="for"){
      curCode=RCTranslation(varInfo,curExp[[4]])
      loopInd=curExp[[2]]
      loopRange=curExp[[3]]
      curVar=getVarInfo(varInfo,loopInd)
      dataType=as.numeric(curVar$precisionType)
      CXXtype=GPUVar$default_index_type
      loopFunc=paste0("for(",CXXtype," ",loopInd,"=",loopRange[[2]],";",
                      loopInd,"<=",loopRange[[3]],";",
                      loopInd,"++){")
      gpu_code=c(gpu_code,loopFunc,curCode,"}")
      next
    }
    if(curExp[[1]]=="if"){
      curCode=RCTranslation(varInfo,curExp[[3]])
      ifFunc=paste0("if(",deparse(curExp[[2]]),"){\n",paste0(curCode,collapse = "\n"),"}")
      elseFunc=""
      if(length(curExp)==4){
      curCode=RCTranslation(varInfo,curExp[[4]])
      elseFunc=paste0("else{",paste0(curCode,collapse = "\n"),"}")
      }
      ifstate=paste0(ifFunc,elseFunc)
      gpu_code=c(gpu_code,ifstate)
      next
    }
  }
  return(gpu_code)
}

