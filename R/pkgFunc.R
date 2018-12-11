#' @importFrom pryr standardise_call
#' @importFrom Deriv Simplify
#' @importFrom Rcpp sourceCpp
#' @importFrom digest digest
#' @importFrom stringr str_match_all
#' @import hash
#' @useDynLib gpuMagic

.onDetach<-function(libpath){
  gc()
}

.onUnload<-function(libpath){
  library.dynam.unload("gpuMagic",libpath)
  .gpuResourcesManager$deleteEnv()
}



DEBUG=TRUE




#' @include RProfilerFunc.R
.profileVarDefine=c("gMatrix","gNumber","subRef")

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
.profileFuncs[["floor"]]=profile_floor
.profileFuncs[["ceiling"]]=profile_ceil
.profileFuncs[["gMatrix"]]=profile_gMatrix
.profileFuncs[["gNumber"]]=profile_gNumber
.profileFuncs[["t"]]=profile_transpose
.profileFuncs[["%*%"]]=profile_matrixMult
.profileFuncs[["subRef"]]=profile_subRef






#' @include RRecompileFunc.R
.recompileFuncs=list()
.recompileFuncs$matrix=recompile_matrix
#.recompileFuncs[["%*%"]]=recompile_matrixMult

#' @include RCParserFunc.R
.cFuncs=list()
.cFuncs[["<-+"]]=C_arithmaticOP_right
.cFuncs[["<--"]]=C_arithmaticOP_right
.cFuncs[["<-*"]]=C_arithmaticOP_right
.cFuncs[["<-/"]]=C_arithmaticOP_right
.cFuncs[["<-matrix"]]=C_matrix_right
.cFuncs[["<-length"]]= C_length_left_right
.cFuncs[["<-nrow"]]= C_nrow_left_right
.cFuncs[["<-ncol"]]= C_ncol_left_right
.cFuncs[["<-["]]=C_subset_right
.cFuncs[["<-floor"]]=C_floor_right
.cFuncs[["<-ceiling"]]=C_ceil_right
.cFuncs[["<-gMatrix"]]=C_NULL
.cFuncs[["<-gNumber"]]=C_NULL
.cFuncs[["<-resize"]]=C_NULL
.cFuncs[["<-subRef"]]=C_NULL
.cFuncs[["<-%*%"]]=C_matMul_right


.cFuncs[["return"]]=C_return
.cFuncs[["break"]]=C_break
.cFuncs[["next"]]=C_next
.cFuncs[["message"]]=C_message
.cFuncs[["setVersion"]]=C_setVersion



.cFuncs[["length<-"]]= C_length_left_right
.cFuncs[["nrow<-"]]= C_nrow_left_right
.cFuncs[["ncol<-"]]= C_ncol_left_right



#' @include gpuResourcesManager.R

T_scale="scale"
T_matrix="matrix"
GPUVar<-local({
  GPUVar_env=new.env()
  #worker private data, loacted in global memory
  GPUVar_env$global_private_data="gpu_gp_data"
  GPUVar_env$global_private_totalSize="gpu_gp_totalSize"
  GPUVar_env$global_private_matrixNum="gpu_gp_matrixNum"
  
  #Per worker length
  GPUVar_env$global_private_size1="gpu_gp_size1"
  GPUVar_env$global_private_size2="gpu_gp_size2"
  #Per worker offset
  GPUVar_env$global_private_offset="gpu_gp_offset"
  
  #worker shared data, located in global memory
  GPUVar_env$global_shared_data="gpu_gs_data"
  GPUVar_env$global_shared_size1="gpu_gs_size1"
  GPUVar_env$global_shared_size2="gpu_gs_size2"
  GPUVar_env$global_shared_offset="gpu_gs_offset"
  
  #worker private data, located in private/local memory
  GPUVar_env$local_private_data="gpu_lp_data"
  GPUVar_env$local_private_size1="gpu_lp_size1"
  GPUVar_env$local_private_size2="gpu_lp_size2"
  GPUVar_env$local_private_offset="gpu_lp_offset"
  
  #worker shared data, located in local memory
  GPUVar_env$local_shared_data="gpu_ls_data"
  GPUVar_env$local_shared_size1="gpu_ls_size1"
  GPUVar_env$local_shared_size2="gpu_ls_size2"
  GPUVar_env$local_shared_offset="gpu_ls_offset"
  
  
  #return value
  GPUVar_env$return_variable="gpu_return_variable"
  #Per worker size
  GPUVar_env$return_size="gpu_return_size"
  
  #The vector that is looped on
  GPUVar_env$gpu_loop_data="gpu_loop_data"
  
  
  #Deducted variable
  GPUVar_env$gpu_global_id="gpu_global_id"
  
  #The offset to find the worker data space in the global memory
  #It is not an argument
  GPUVar_env$worker_offset="gpu_worker_offset"
  
  #parameters for creating the function
  GPUVar_env$functionCount=0
  GPUVar_env$functionName="gpu_kernel"
  
  #This number can be reset to 0 in the beggining of the parser
  #The parser can call it when it needs a new variable
  GPUVar_env$tempVarInd=0
  GPUVar_env$getTmpVar<-function(){
    GPUVar_env$tempVarInd=GPUVar_env$tempVarInd+1
    return(paste0("gpu_temp_var",GPUVar_env$tempVarInd))
  }
  GPUVar_env$resetTmpCount<-function(){
    GPUVar_env$tempVarInd=0
  }
  
  GPUVar_env$default_index_type="uint"
  
  #The for loop index
  GPUVar_env$gpu_loop_ind="gpu_loop_ind"
  
  #c(global_private_totalSize,global_private_matrixNum,return_size)
  GPUVar_env$size_info="gpu_sizeInfo"
  
  GPUVar_env$preservedFuncPrefix="compiler."
  GPUVar_env$openclCode=".opencl_"
  GPUVar_env$openclFuncCall=".opencl("
  
  #This variable is for doing the matrix optimization
  GPUVar_env$private_var_space="gpu_private_spcae"
  GPUVar_env$private_size=32
  
  
  return(GPUVar_env)
})


