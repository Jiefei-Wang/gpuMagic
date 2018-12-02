gpuMagic.option<-local({
  e=new.env()
  e$supportedType<-c("char","half","float","double","int","long","uint","ulong")
  
  e$default.float="double"
  e$default.int="int"
  e$default.type="double"
  
 
  
  list(
    getAvailableType=function(){e$supportedType},
    getDefaultFloat=function(){e$default.float},
    getDefaultInt=function(){e$default.int},
    getDefaultType=function(){e$default.type},
    getDefaultIndexType=function(){GPUVar$default_index_type},
    setDefaultFloat=function(type){
      checkTypeSupport(type)
      e$default.float=type
    },
    setDefaultInt=function(type){
      checkTypeSupport(type)
      e$default.int=type
    },
    setDefaultType=function(type){
      checkTypeSupport(type)
      e$default.type=type
    },
    setDefaultIndexType=function(type){
      checkTypeSupport(type)
      GPUVar$default_index_type=type
    }
  )
})









