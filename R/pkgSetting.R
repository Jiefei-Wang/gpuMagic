#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom Rcpp sourceCpp
#' @importFrom digest digest
#' @importFrom stringr str_match_all
#' @importFrom DescTools StrAlign
#' @importFrom future future value
#' @import hash
#' @useDynLib gpuMagic, .registration = TRUE

.onDetach<-function(libpath){
  gc()
}

.onUnload<-function(libpath){
  .gpuResourcesManager$deleteEnv()
  library.dynam.unload("gpuMagic",libpath)
}
.onLoad<-function(libname, pkgname){
  setDevice(1)
}

DEBUG=TRUE
gpuMagic<-local({
  e=new.env()
  e$supportedType<-c("bool","char","half","float","double","int","long","uint","ulong")
  
  
  e$options=data.frame(default.float="double",
                       default.int="int",
                       default.index.type="uint",
                       default.thread.num=64,
                       stringsAsFactors=F
  )
  
  
  list(
    getOptions=function(opt="all",print=T){
      if(opt=="all")
        curOpt=e$options
      else
        curOpt=e$options[,opt,drop=F]
      if(print){
        prettyPrint(curOpt)
      }else{
        return(curOpt)
      }
    },
    setOptions=function(...){
      parms=list(...)
      optNames=names(parms)
      for(i in optNames){
        value=parms[[i]]
        switch(i,
               default.float={
                 checkTypeSupport(value)
                 e$options$default.float=value
                 GPUVar$default_float=value
               },
               default.int={
                 checkTypeSupport(value)
                 e$options$default.int=value
                 GPUVar$default_int=value
               },
               default.index.type={
                 checkTypeSupport(value)
                 e$options$default.index.type=value
                 GPUVar$default_index_type=value
               },
               default.threadNum={
                 if(is.numeric(num))
                   e$options$default.thread.num=value
                 else
                   stop("The function argument should be a numeric value")
               }
        )
      }
    },
    getAvailableType=function(){
      e$supportedType
    }
  )
})






