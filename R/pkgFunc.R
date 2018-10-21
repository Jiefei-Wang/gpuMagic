.onUnload<-function(libpath){
  library.dynam.unload("gpuMagic",libpath)
  .gpuResourcesManager$deleteEnv()
}

#' @importFrom pryr standardise_call