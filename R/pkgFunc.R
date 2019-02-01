T_scale="scale"
T_matrix="matrix"
GPUVar<-local({
  GPUVar_env=new.env()
  
  #The precision setting
  GPUVar_env$default_index_type="uint"
  GPUVar_env$default_float="double"
  GPUVar_env$default_int="int"
  GPUVar_env$default_size_type="uint"
  
  #matrix size info
  GPUVar_env$global_private_size="gpu_gp_size_arg"
  GPUVar_env$global_share_size="gpu_gs_size_arg"
  GPUVar_env$local_share_size="gpu_ls_size_arg"
  
  #matrix number info
  GPUVar_env$global_private_matrixNum="gpu_gp_matrixNum"
  GPUVar_env$global_share_matrixNum="gpu_gs_matrixNum"
  GPUVar_env$local_share_matrixNum="gpu_ls_matrixNum"
  GPUVar_env$local_private_matrixNum="gpu_lp_matrixNum"
  
  #matrix transpose info
  GPUVar_env$gp_transpose="gpu_gp_transpose"
  GPUVar_env$gs_transpose="gpu_gs_transpose"
  GPUVar_env$lp_transpose="gpu_lp_transpose"
  GPUVar_env$ls_transpose="gpu_ls_transpose"
  
  
  #worker private data, loacted in global memory
  GPUVar_env$global_private_data="gpu_gp_data"
  GPUVar_env$global_private_totalSize="gpu_gp_totalSize"
  
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
  
  
  
  #c(global_private_totalSize,global_private_matrixNum,return_size)
  GPUVar_env$size_info="gpu_sizeInfo"
  
  GPUVar_env$preservedFuncPrefix="compiler."
  GPUVar_env$openclCode=".opencl_"
  GPUVar_env$openclFuncCall=".opencl("
  
  #The shared size in byte
  #For doing the matrix multiplication
  GPUVar_env$vectorSize=8
  GPUVar_env$shared_size=8*1024
  
  
  return(GPUVar_env)
})




.elementFuncs=c(
  "+","-","*","/"
  )
.elementTransformation=c(
  "floor","ceiling"
)
.elementOp=c(.elementFuncs,.elementTransformation)

#' @include RProfilerFunc.R

.profileFuncs=list()
.profileFuncs[["nrow"]]=profile_size
.profileFuncs[["ncol"]]=profile_size
.profileFuncs[["length"]]=profile_size
.profileFuncs[["matrix"]]=profile_matrix
.profileFuncs[["+"]]=profile_arithmetic
.profileFuncs[["-"]]=profile_arithmetic
.profileFuncs[["*"]]=profile_arithmetic
.profileFuncs[["/"]]=profile_arithmetic
.profileFuncs[[">"]]=profile_logical
.profileFuncs[[">="]]=profile_logical
.profileFuncs[["<"]]=profile_logical
.profileFuncs[["<="]]=profile_logical
.profileFuncs[["=="]]=profile_logical
.profileFuncs[["["]]=profile_subset
.profileFuncs[["floor"]]=profile_floor
.profileFuncs[["ceiling"]]=profile_ceil
.profileFuncs[["gMatrix"]]=profile_gMatrix
.profileFuncs[["gNumber"]]=profile_gNumber
.profileFuncs[["t"]]=profile_transpose
.profileFuncs[["%*%"]]=profile_matrixMult
.profileFuncs[["subRef"]]=profile_subRef
.profileFuncs[["seq"]]=profile_seq
.profileFuncs[[":"]]=profile_oneStepSeq






#' @include RRecompileFunc.R
.recompileFuncs=list()
.recompileFuncs$matrix=recompile_matrix
#.recompileFuncs[["%*%"]]=recompile_matrixMult

#' @include RCParserFunc.R
.cFuncs=list()
.cFuncs[["<-matrix"]]=C_matrix_right
.cFuncs[["<-length"]]= C_length_left_right
.cFuncs[["<-nrow"]]= C_nrow_left_right
.cFuncs[["<-ncol"]]= C_ncol_left_right
.cFuncs[["<-["]]=C_subset_right
.cFuncs[["<-gMatrix"]]=C_NULL
.cFuncs[["<-gNumber"]]=C_NULL
.cFuncs[["<-resize"]]=C_NULL
.cFuncs[["<-subRef"]]=C_NULL
.cFuncs[["<-%*%"]]=C_matMul_right
.cFuncs[["<-seq"]]=C_seq_right
.cFuncs[["<-:"]]=C_oneStepSeq_right

#Element op
.cFuncs[["<-+"]]=C_element_arithmatic
.cFuncs[["<--"]]=C_element_arithmatic
.cFuncs[["<-*"]]=C_element_arithmatic
.cFuncs[["<-/"]]=C_element_arithmatic
.cFuncs[["<-floor"]]=C_element_floor
.cFuncs[["<-ceiling"]]=C_element_ceil



.cFuncs[["length<-"]]= C_length_left_right
.cFuncs[["nrow<-"]]= C_nrow_left_right
.cFuncs[["ncol<-"]]= C_ncol_left_right


.cFuncs[["return"]]=C_return
.cFuncs[["break"]]=C_break
.cFuncs[["next"]]=C_next
.cFuncs[["message"]]=C_message
.cFuncs[["setVersion"]]=C_setVersion



#' Create a scalar variable
#' 
#' The function will create a scalar variable, it is only useful in the openCL functions. 
#' It can also be called in R, but its argument will not take any effect.
#' 
#' @param precision The variable type, please refer to `gpuMagic.getAvailableType()` to see the available data type.
#' @param constDef 
#' Specify if the variable can be redefined. The package will automatically update the variable definition when it is needed, 
#' if you do not need this feature, you can manually turn the feature off. 
#' It is useful in some special cases such as turning off the auto update to do the integer division
#' (By default, the package will convert the variable to the default float type before doing the division).
#' 
#' @examples 
#' a=gNumber(precision="double",constDef=FALSE)
#' 
#' @return a variable initialize with 0.
#' @export
gNumber<-function(precision=GPUVar$default_float,constDef=FALSE){
  return(0)
}
#' Create a matrix
#' 
#' The function create a matrix, it is only useful in the openCL functions. 
#' it can also be called in R, but its argument may or may not take any effect.
#' 
#' @param nrow,ncol The matrix dimension.
#' @param precision The variable type, please refer to `gpuMagic.getAvailableType()` to see the available data type.
#' @param constDef 
#' Specify if the variable can be redefined. The package will automatically update the variable definition when it is needed, 
#' if you do not need this feature, you can manually turn the feature off. 
#' It is useful in some special cases such as turning off the auto update to do the integer division
#' (By default, the package will convert the variable to the default float type before doing the division).
#' @param shared If the matrix is shared by all the workers in a work group. Do not use it if you don't know its meaning.
#' @param location The physical memory location of the matrix, it can be either "global" or "local". Do not use it if you don't know its meaning.
#' @examples 
#' #Create a 10-by-10 matrix
#' A=gMatrix(10,10)
#' @return a matrix initialize with 0.
#' @export
gMatrix<-function(nrow=1,ncol=1,precision=GPUVar$default_float,constDef=FALSE,shared=FALSE,location="global"){
  return(matrix(NA,nrow,ncol))
}
# TODO 
resize<-function(data,nrow,ncol){
  return(matrix(data,nrow,ncol))
}
#' Get a reference of the subset of a matrix 
#' 
#' The function will get a reference of the matrix subset. This is a 0-copy method, 
#' which means any change in the reference variable will cause the change in the original matrix.
#' The function is useful when the GPU memory is limited or you do not want to create a copy the data. 
#' DO NOT call this function in R, this is for openCL code only(eg. gpuSapply).
#' 
#' The package implement this function purely using the code. it will not actually be called on device side. 
#' For example, if we have the following code:
#' 
#' \preformatted{
#' #Alternative of B=A[ind]
#' B=subRef(A,ind)
#' a=B[2]
#' }
#' 
#' In the compilation stage, the code will be changed to
#' 
#' \preformatted{
#' a=A[ind[2]]
#' }
#' 
#' The variable B does not exist in the code after the compilation and therefore no memory is allocated for it. 
#' 
#' @section Warning:
#' Since this feature is implemented like a macro, 
#' so it is possible to change the value of `ind` after the matrix B is created and before you modify the matrix B.
#' In such case, it may cause an unexpected error.
#' It is a good practice to keep the `ind` same while using the subset reference.
#'  
#' 
#' @param variable the matrix you want to subset
#' @param i the index of a vector or the row index of a matrix
#' @param j (Optional) The column index of a matrix
#' @examples 
#' #create data
#' ind=1:10
#' A=matrix(0,100,100)
#' #Use the one-index subsetting, create a vector of length 10
#' B=subRef(A,ind)
#' #Subsetting the matrix A,create a 10-by-10 matrix
#' C=subRef(A,ind,ind)
#' #row subsetting 
#' D=subRef(A,ind,)
#' #column subsetting
#' E=subRef(A,,ind)
#' @return A reference to the subset of a matrix
#' @export
subRef<-function(variable,i="",j=""){
  if(length(i)==1&&length(j)==1&&i==""&&j=="")
    return(variable[,,drop=FALSE])
  if(length(i)==1&&i=="")
    return(variable[,j,drop=FALSE])
  if(length(j)==1&&j=="")
    return(variable[i,,drop=FALSE])
  return(variable[i,j,drop=FALSE])
}
#' Return the result without memory copy
#' 
#' The usage of the `return.nocpy` is same as `return`. This feature is for openCL code only, please not use it in R function.
#' 
#' @param x The return value
#' @return No return value
#' @export
return.nocpy<-function(x){
  stop("You cannot use the reference return in R code")
}
