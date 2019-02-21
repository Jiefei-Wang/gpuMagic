#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom digest digest
#' @importFrom stringr str_match_all
#' @importFrom DescTools StrAlign
#' @importFrom utils ls.str
#' @importFrom utils capture.output
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
    # setDevice(1)
}

DEBUG = TRUE

