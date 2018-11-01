#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom Rcpp sourceCpp
#' @import hash
#' @useDynLib gpuMagic

.onUnload<-function(libpath){
  library.dynam.unload("gpuMagic",libpath)
  .gpuResourcesManager$deleteEnv()
}



DEBUG=TRUE

#' @include RProfilerFunc.R
.profileFuncs=list()
.profileFuncs$nrow=profile_size
.profileFuncs$ncol=profile_size
.profileFuncs$length=profile_size
.profileFuncs$matrix=profile_matrix
.profileFuncs$"+"=profile_arithmetic
.profileFuncs$"-"=profile_arithmetic
.profileFuncs$"*"=profile_arithmetic
.profileFuncs$"/"=profile_arithmetic
.profileFuncs$"["=profile_subset
.profileFuncs$floor=profile_floor
.profileFuncs$gMatrix=profile_gMatrix
.profileFuncs$gNumber=profile_gNumber


#' @include RRecompileFunc.R
.recompileFuncs=list()
.recompileFuncs$matrix=recompile_matrix


#' @include RCParserFunc.R
.cFuncs=list()
.cFuncs$"["=C_subset
.cFuncs$"+"=C_arithmaticOP
.cFuncs$"-"=C_arithmaticOP
.cFuncs$"*"=C_arithmaticOP
.cFuncs$"/"=C_arithmaticOP
.cFuncs$length= C_length 
.cFuncs$nrow= C_nrow
.cFuncs$ncol= C_ncol
.cFuncs$floor=C_floor
.cFuncs$matrix=C_NULL
.cFuncs$gMatrix=C_NULL
.cFuncs$gNumber=C_NULL
.cFuncs$resize=C_NULL






#' @include gpuResourcesManager.R

T_scale="scale"
T_matrix="matrix"
GPUVar<-local({
  GPUVar_env=new.env()
  GPUVar_env$gpu_tmp_var="gpuMagic_tmp"
  #Per worker length
  GPUVar_env$gpu_tmp_length_arg="gpu_tmp_length_arg"
  GPUVar_env$gpu_matrix_size1="gpu_matrix_size1"
  GPUVar_env$gpu_matrix_size2="gpu_matrix_size2"
  GPUVar_env$gpu_return_variable="gpu_return_variable"
  #Per worker size
  GPUVar_env$gpu_return_size="gpu_return_size"
  #The vector that is looped on
  GPUVar_env$gpu_worker_data="gpu_worker_data"
  
  
  #Deducted variable
  GPUVar_env$gpu_tmp_matrix_offSize="gpu_matrix_offSize"
  GPUVar_env$gpu_global_id="gpu_global_id"
  GPUVar_env$gpu_tmp_length="gpu_tmp_length"
  GPUVar_env$gpu_worker_offset="gpu_worker_offset"
  
  #parameters
  GPUVar_env$functionCount=0
  GPUVar_env$functionName="gpu_kernel"
  GPUVar_env$default_index_type="unsigned int"
  
  GPUVar_env$gpu_loop_ind="gpu_loop_ind"
  GPUVar_env$private_mem="private_mem_preserved"
  return(GPUVar_env)
})


