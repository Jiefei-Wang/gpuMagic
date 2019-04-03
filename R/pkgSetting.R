#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom digest digest
#' @importFrom DescTools StrAlign
#' @importFrom utils capture.output ls.str
#' @importFrom stringr str_match str_match_all str_extract_all
#' @import methods
#' @import BiocGenerics
#' @useDynLib gpuMagic, .registration = TRUE,  .fixes = 'C_'

.onDetach <- function(libpath) {
    gc()
}

.onUnload <- function(libpath) {
    .gpuResourcesManager$deleteEnv()
    library.dynam.unload("gpuMagic", libpath)
}
.onLoad <- function(libname, pkgname) {
    if(getPlatformNum()!=0){
      setDevice(1)
    }
}

DEBUG = TRUE

