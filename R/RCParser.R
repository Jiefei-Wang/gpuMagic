#' @include pkgFunc.R

RCcompilerLevel1<-function(profileMeta3){
  if(DEBUG){
    profileMeta3$varInfo=copyVarInfoTbl(profileMeta3$varInfo)
  }
  tmpMeta=profileMeta3$tmpMeta
  parsedExp=profileMeta3$Exp
  varInfo=profileMeta3$varInfo
  profile=varInfo$profile
  
  #Preserved variables
  #Global worker private data
  gpu_gp_data=GPUVar$global_private_data
  #Per worker length
  gpu_gp_totalSize=GPUVar$global_private_totalSize
  gpu_gp_matrixNum=GPUVar$global_private_matrixNum
  
  gpu_gp_size1=GPUVar$global_private_size1
  gpu_gp_size2=GPUVar$global_private_size2
  #matrix index offset
  gpu_gp_offset=GPUVar$global_private_offset
  
  #worker shared data, located in global memory
  gpu_gs_data=GPUVar$global_shared_data
  gpu_gs_size1=GPUVar$global_shared_size1
  gpu_gs_size2=GPUVar$global_shared_size2
  gpu_gs_offset=GPUVar$global_shared_offset
  
  #worker private data, located in private/local memory
  gpu_lp_data=GPUVar$local_private_data
  gpu_lp_size1=GPUVar$local_private_size1
  gpu_lp_size2=GPUVar$local_private_size2
  gpu_lp_offset=GPUVar$local_private_offset
  
  #worker shared data, located in local memory
  gpu_ls_data=GPUVar$local_shared_data
  gpu_ls_size1=GPUVar$local_shared_size1
  gpu_ls_size2=GPUVar$local_shared_size2
  gpu_ls_offset=GPUVar$local_shared_offset
  
  gpu_sizeInfo=GPUVar$size_info
  
  gpu_returnSize=GPUVar$return_size
  
  #Deducted variable
  gpu_global_id=GPUVar$gpu_global_id
  gpu_worker_offset=GPUVar$worker_offset
  
  gpu_code=c(
    paste0(GPUVar$default_index_type," ",gpu_global_id,"=get_global_id(0);"),
    paste0(GPUVar$default_index_type," ", gpu_gp_totalSize,"=",gpu_sizeInfo,"[0];"),
    paste0(GPUVar$default_index_type," ", gpu_gp_matrixNum,"=",gpu_sizeInfo,"[1];"),
    paste0("global ",GPUVar$default_index_type,"* ", gpu_gp_size1,"=",gpu_gp_size1,"_arg+",gpu_gp_matrixNum,"*",gpu_global_id,";"),
    paste0("global ",GPUVar$default_index_type,"* ", gpu_gp_size2,"=",gpu_gp_size2,"_arg+",gpu_gp_matrixNum,"*",gpu_global_id,";"),
    paste0(GPUVar$default_index_type," ", gpu_worker_offset,"=",gpu_global_id,"*",gpu_gp_totalSize,";"),
    paste0(GPUVar$default_index_type," ", gpu_returnSize,"=",gpu_sizeInfo,"[2];")
  )
  
  gpu_gp_num=-1
  gpu_gs_num=-1
  gpu_lp_num=-1
  gpu_ls_num=-1
  #matrixInd is for finding the index of a matrix size in the gpu code
  varInfo$matrixInd=hash()
  varInfo$matrix_gp=c()
  varInfo$matrix_gs=c()
  varInfo$matrix_lp=c()
  varInfo$matrix_ls=c()
  
  for(curVar in keys(varInfo$varVersion)){
    curInfo=getVarInfo(varInfo,curVar,1)
    #if(curInfo$dataType==T_matrix)
    #  gpu_matrix_num=gpu_matrix_num+1
    if(curInfo$lazyRef){
      next
    }
    
    if(curInfo$location!="local"||curInfo$shared)
      curInfo$dataType=T_matrix
    
    if(!curInfo$initialization){
      curInfo$address=curInfo$var
      if(curInfo$require){
        gpu_gs_num=gpu_gs_num+1
        varInfo$matrixInd[[curVar]]=gpu_gs_num
        varInfo$matrix_gs=c(varInfo$matrix_gs,curVar)
      }
      varInfo=setVarInfo(varInfo,curInfo)
      next
    }
    if(curInfo$dataType==T_scale){
      CXXtype=curInfo$precisionType
      curCode=paste0(CXXtype," ",curInfo$var,";")
      gpu_code=c(gpu_code,curCode)
      curInfo$address=curInfo$var
      varInfo=setVarInfo(varInfo,curInfo)
      next
    }
    
    if(curInfo$dataType==T_matrix){
      
      CXXtype=curInfo$precisionType
      size1=curInfo$size1
      size2=curInfo$size2
    if(curInfo$location=="global"&&curInfo$shared){
      gpu_gs_num=gpu_gs_num+1
      varInfo$matrixInd[[curVar]]=gpu_gs_num
      varInfo$matrix_gs=c(varInfo$matrix_gs,curVar)
      curCode=paste0("global ",CXXtype,"* ",curInfo$var,"=",
                     "(global ",CXXtype,"*)(",
                     gpu_gs_data
                     ,"+",
                     gpu_gs_offset,"[",gpu_gs_num,"]",");")
    }
    if(curInfo$location=="local"&&curInfo$shared){
      gpu_ls_num=gpu_ls_num+1
      varInfo$matrixInd[[curVar]]=gpu_ls_num
      varInfo$matrix_ls=c(varInfo$matrix_ls,curVar)
      curCode=paste0("local ",CXXtype,"* ",curInfo$var,"=",
                     "(local ",CXXtype,"*)(",
                     gpu_ls_data
                     ,"+",
                     gpu_ls_offset,"[",gpu_ls_num,"]",");")
    }
    if(curInfo$location=="global"&&!curInfo$shared){
      gpu_gp_num=gpu_gp_num+1
      varInfo$matrixInd[[curVar]]=gpu_gp_num
      varInfo$matrix_gp=c(varInfo$matrix_gp,curVar)
      curCode=paste0("global ",CXXtype,"* ",curInfo$var,"=",
                     "(global ",CXXtype,"*)(",
                     gpu_gp_data
                     ,"+",gpu_worker_offset,"+",
                     gpu_gp_offset,"[",gpu_gp_num,"]",");")
    }
    if(curInfo$location=="local"&&!curInfo$shared){
      gpu_lp_num=gpu_lp_num+1
      varInfo$matrixInd[[curVar]]=gpu_lp_num
      varInfo$matrix_lp=c(varInfo$matrix_lp,curVar)
      curCode=paste0("private ",CXXtype,"* ",curInfo$var,"=",
                     "(private ",CXXtype,"*)(",
                     gpu_lp_data
                     ,"+",
                     gpu_lp_offset,"[",gpu_lp_num,"]",");")
    }
    gpu_code=c(gpu_code,curCode)
    curInfo$address=curInfo$var
    curInfo$dataType=T_matrix
    varInfo=setVarInfo(varInfo,curInfo)
    next
    }
    
  }
  
  
  
  #gpu_code=c(gpu_code,paste0(getTypeCXXStr(T_DEFAULT_float)," ",profileMeta3$workerData,";"))
  #gpu_code=c(gpu_code,paste0(profileMeta3$workerData,"=",GPUVar$gpu_worker_data,"[",gpu_global_id,"];"))
  #gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
             
  GPUExp1=profileMeta3
  GPUExp1$tmpMeta=tmpMeta
  GPUExp1$Exp=parsedExp
  GPUExp1$varInfo=varInfo
  GPUExp1$gpu_code=gpu_code
  
      
  return(GPUExp1)
}


RCcompilerLevel2<-function(GPUExp1){
  if(DEBUG){
    GPUExp1$varInfo=copyVarInfoTbl(GPUExp1$varInfo)
    if(length(GPUExp1$varInfo$matrixInd)!=0)
      GPUExp1$varInfo$matrixInd=copy(GPUExp1$varInfo$matrixInd)
  }
  
  parsedExp=GPUExp1$Exp
  varInfo=GPUExp1$varInfo
  gpu_code=GPUExp1$gpu_code
  gpu_code=c(gpu_code,RCTranslation(varInfo,parsedExp))
  
  GPUExp2=GPUExp1
  GPUExp2$gpu_code=gpu_code
  
  GPUExp2
}



RCTranslation<-function(varInfo,parsedExp){
  gpu_code=c()
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    if(curExp=="{"||is.symbol(curExp))
      next
    
    
    
    if(curExp[[1]]=="for"){
      curCode=RCTranslation(varInfo,curExp[[4]])
      loopInd=curExp[[2]]
      loopRange=curExp[[3]]
      curVar=getVarInfo(varInfo,loopInd)
      CXXtype=curVar$precisionType
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
    
    #If the code starts with opencl_
    code_char=deparse(curExp)[1]
    if(substr(code_char,1,7)==GPUVar$openclCode){
      code_char=deparse(curExp)
      curCode=paste0(substr(code_char,8,nchar(code_char)),";")
      gpu_code=c(gpu_code,curCode)
      next
    }
    #if the code format exactly match the template
    formattedExp=formatCall(curExp,generalType=FALSE)
    formattedExp_char=gsub(" ", "",deparse(formattedExp), fixed = TRUE)
    func=.cFuncs[[formattedExp_char]]
    if(!is.null(func)){
      curCode=func(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
      next
    }
    
    #if the code format exactly match the general type template
    formattedExp=formatCall(curExp,generalType=TRUE)
    formattedExp_char=gsub(" ", "",deparse(formattedExp), fixed = TRUE)
    func=.cFuncs[[formattedExp_char]]
    if(!is.null(func)){
      curCode=func(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
      next
    }
    #if the code does not exactly match the template but has an equal sign, partially match is used
    if(switch(deparse(curExp[[1]]),"="=T,"=="=T,F)){
      curCode=C_call_assign(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
      next
    }
    
    func=.cFuncs[[deparse(curExp[[1]])]]
    if(!is.null(func)){
      curCode=func(varInfo,curExp)
      gpu_code=c(gpu_code,curCode)
      next
    }
    
    
    
    
    
  }
  return(gpu_code)
}

