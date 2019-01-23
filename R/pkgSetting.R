#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom Rcpp sourceCpp
#' @importFrom digest digest
#' @importFrom stringr str_match_all
#' @importFrom DescTools StrAlign
#' @importFrom future future value
#' @import hash
#' @import methods
#' @import BiocGenerics
#' @useDynLib gpuMagic, .registration = TRUE,  .fixes = "C_"

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


  

