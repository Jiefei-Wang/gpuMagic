gpuMagic.option<-local({
  e=new.env()
  e$supportedType<-c("char","half","float","double","int","long","uint","ulong")
  
  e$default.float="double"
  e$default.int="int"
  e$default.threadNum=64
 
  
  list(
    getAvailableType=function(){e$supportedType},
    getDefaultFloat=function(){e$default.float},
    getDefaultInt=function(){e$default.int},
    getDefaultIndexType=function(){GPUVar$default_index_type},
    getDefaultThreadNum=function(){e$default.threadNum},
    
    setDefaultFloat=function(type){
      checkTypeSupport(type)
      e$default.float=type
    },
    setDefaultInt=function(type){
      checkTypeSupport(type)
      e$default.int=type
    },
    setDefaultIndexType=function(type){
      checkTypeSupport(type)
      GPUVar$default_index_type=type
    },
    setDefaultThreadNum=function(num){
      if(is.numeric(num))
        e$default.threadNum=num
      else
        stop("The function argument should be a numeric value")
    }
  )
})









