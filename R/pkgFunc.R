T_scale = "scale"
T_matrix = "matrix"
GPUVar <- local({
    GPUVar_env = new.env()
    
    # The precision setting
    GPUVar_env$default_index_type = "uint"
    GPUVar_env$default_float = "double"
    GPUVar_env$default_int = "int"
    GPUVar_env$default_size_type = "uint"
    
    
    #gpu size prefix
    GPUVar_env$matrix_size_prefix="gpu_s_"
    
    #Promise series
    GPUVar_env$matrix_temporary_space="gpu_matrix_temporary_space"
    GPUVar_env$promiseAssgin="//compiler promise assign--"
    GPUVar_env$promiseDef="//compiler promise define--"
    
    #Matrix number
    GPUVar_env$gp_number="gpu_gp_number"
    GPUVar_env$gs_number="gpu_gs_number"
    GPUVar_env$lp_number="gpu_lp_number"
    GPUVar_env$ls_number="gpu_ls_number"
    
    
    #This is the offset to find the data in the function argument
    #It is a macro
    GPUVar_env$global_private_offset="gpu_gp_offset"
    GPUVar_env$global_shared_offset="gpu_gs_offset"
    GPUVar_env$local_shared_offset="gpu_ls_offset"
    
    #The macro that find the gp,gs,lp,ls size
    GPUVar_env$gp_size="gpu_gp_size"
    GPUVar_env$gs_size="gpu_gs_size"
    GPUVar_env$lp_size="gpu_lp_size"
    GPUVar_env$ls_size="gpu_ls_size"
    
    
    
    # matrix size info
    GPUVar_env$global_private_size = "gpu_gp_size_arg"
    GPUVar_env$global_share_size = "gpu_gs_size_arg"
    GPUVar_env$local_share_size = "gpu_ls_size_arg"
    
    
    # worker private data, loacted in global memory
    GPUVar_env$global_private_data = "gpu_gp_data"
    
    # Per worker offset
    GPUVar_env$global_private_offset = "gpu_gp_offset"
    
    # worker shared data, located in global memory
    GPUVar_env$global_shared_data = "gpu_gs_data"
    
    # worker private data, located in private/local memory
    GPUVar_env$local_private_data = "gpu_lp_data"
    
    # worker shared data, located in local memory
    GPUVar_env$local_shared_data = "gpu_ls_data"
    
    
    # return value
    GPUVar_env$return_variable = "gpu_return_variable"
    # Per worker size
    GPUVar_env$return_size = "gpu_return_size"
    
    # The vector that is looped on
    GPUVar_env$gpu_loop_data = "gpu_loop_data"
    
    
    # Deducted variable
    GPUVar_env$gpu_global_id = "gpu_global_id"
    GPUVar_env$gpu_global_size = "gpu_global_size"
    
    
    # The offset to find the worker data space in the global memory 
    # It is not an argument
    GPUVar_env$worker_offset = "gpu_worker_offset"
    GPUVar_env$element_dist="gpu_element_dist"
    
    # parameters for creating the function
    GPUVar_env$functionCount = 0
    GPUVar_env$functionName = "gpu_kernel"
    
    GPUVar_env$variableDef="GPU_VARIABLE_DEF_"
    
    
    # This number can be reset to 0 in the beggining of the parser The
    # parser can call it when it needs a new variable
    GPUVar_env$tempVarInd = 0
    GPUVar_env$getTmpVar <- function() {
        GPUVar_env$tempVarInd = GPUVar_env$tempVarInd + 1
        return(paste0("gpu_temp_var", GPUVar_env$tempVarInd))
    }
    GPUVar_env$resetTmpCount <- function() {
        GPUVar_env$tempVarInd = 0
    }
    
    
    
    # c(global_private_totalSize,global_private_matrixNum,return_size)
    GPUVar_env$size_info = "gpu_sizeInfo"
    
    GPUVar_env$preservedFuncPrefix = "compiler."
    GPUVar_env$openclCode = ".opencl_"
    GPUVar_env$openclFuncCall = ".opencl("
    
    
    GPUVar_env$parmsTblName="parms"
    
    # The shared size in byte For doing the matrix multiplication
    GPUVar_env$vectorSize = 4
    # in byte
    GPUVar_env$private_vector_size = 24 * 1024
    
    
    
    
    return(GPUVar_env)
})




.elementFuncs = c("+", "-", "*", "/","^", 
                  ">", ">=", "<", "<=", "==","!=",
                  "abs","abs_int","abs_float","(","[",
                  "nrow","ncol","length","floor", "ceiling","sweep")

.elementOp = c(.elementFuncs)
#These functions will be dispatched to the regular expression translation
.noParentElementOP = c("t",
                       "sum","rowSums","colSums","rowMeans","colMeans" ,
                       "return","seq")
.noChildElementOP = c()

#' @include RProfilerFunc.R

.profileFuncs = list()
.profileFuncs[["nrow"]] = profile_size
.profileFuncs[["ncol"]] = profile_size
.profileFuncs[["length"]] = profile_size
.profileFuncs[["matrix"]] = profile_matrix
.profileFuncs[["["]] = profile_subset
.profileFuncs[["floor"]] = profile_floor
.profileFuncs[["ceiling"]] = profile_ceil
.profileFuncs[["Matrix"]] = profile_Matrix
.profileFuncs[["Scalar"]] = profile_Scalar
.profileFuncs[["t"]] = profile_transpose
.profileFuncs[["t_nocpy"]] = profile_transpose_nocpy
.profileFuncs[["sum"]]=profile_sum
.profileFuncs[["rowSums"]]=profile_rowSums
.profileFuncs[["colSums"]]=profile_colSums
.profileFuncs[["rowMeans"]]=profile_rowMeans
.profileFuncs[["colMeans"]]=profile_colMeans
.profileFuncs[["sweep"]]=profile_sweep
.profileFuncs[["("]]=profile_parenthesis


#element op
.profileFuncs[["+"]] = profile_arithmetic
.profileFuncs[["-"]] = profile_arithmetic
.profileFuncs[["*"]] = profile_arithmetic
.profileFuncs[["/"]] = profile_arithmetic
.profileFuncs[["^"]] = profile_arithmetic
.profileFuncs[[">"]] = profile_logical
.profileFuncs[[">="]] = profile_logical
.profileFuncs[["<"]] = profile_logical
.profileFuncs[["<="]] = profile_logical
.profileFuncs[["=="]] = profile_logical
.profileFuncs[["!="]] = profile_logical
.profileFuncs[["abs"]] = profile_abs
.profileFuncs[["abs_int"]] = profile_abs
.profileFuncs[["abs_float"]] = profile_abs



.profileFuncs[["%*%"]] = profile_matrixMult
.profileFuncs[["subRef"]] = profile_subRef
.profileFuncs[["seq"]] = profile_seq
.profileFuncs[[":"]] = profile_oneStepSeq

.profileCheckFuncs=list()
.profileCheckFuncs[["["]]=profileCheck_subset
.profileCheckFuncs[["nrow"]]=profileCheck_size
.profileCheckFuncs[["ncol"]]=profileCheck_size


# .recompileFuncs[['%*%']]=recompile_matrixMult

#' @include RCParserFunc.R
#' @include RCParser_elementOP.R
.cFuncs = list()
.cFuncs[["<-matrix"]] = C_matrix_right
.cFuncs[["length<-"]] = C_length_left
.cFuncs[["nrow<-"]] = C_nrow_left
.cFuncs[["ncol<-"]] = C_ncol_left
.cFuncs[["<-Matrix"]] = C_NULL
.cFuncs[["<-Scalar"]] = C_NULL
.cFuncs[["<-resize"]] = C_NULL
.cFuncs[["<-subRef"]] = C_NULL
.cFuncs[["<-%*%"]] = C_matMul_right
.cFuncs[["<-seq"]] = C_seq_right
.cFuncs[["<-:"]] = C_oneStepSeq_right
.cFuncs[["<-t"]] = C_transpose_right
.cFuncs[["<-t_nocpy"]] = C_NULL

# No parent opration
.cFuncs[["<-sum"]] = C_sum_right
.cFuncs[["<-rowSums"]] = C_rowSums_right
.cFuncs[["<-colSums"]] = C_colSums_right
.cFuncs[["<-rowMeans"]] = C_rowMeans_right
.cFuncs[["<-colMeans"]] = C_colMeans_right


# Element op
.cFuncs[["<-("]]=C_element_parenthesis
.cFuncs[["<-+"]] = C_element_arithmatic
.cFuncs[["<--"]] = C_element_arithmatic
.cFuncs[["<-*"]] = C_element_arithmatic
.cFuncs[["<-/"]] = C_element_arithmatic
.cFuncs[["<->"]] = C_element_arithmatic
.cFuncs[["<->="]] = C_element_arithmatic
.cFuncs[["<-<"]] = C_element_arithmatic
.cFuncs[["<-<="]] = C_element_arithmatic
.cFuncs[["<-=="]] = C_element_arithmatic
.cFuncs[["<-!="]] = C_element_arithmatic
.cFuncs[["<-^"]] = C_element_arithmatic
.cFuncs[["<-floor"]] = C_element_floor
.cFuncs[["<-ceiling"]] = C_element_ceil
.cFuncs[["<-abs_int"]] = C_element_abs
.cFuncs[["<-abs_float"]] = C_element_abs
.cFuncs[["<-["]] = C_element_sub
.cFuncs[["<-length"]] = C_element_length
.cFuncs[["<-nrow"]] = C_element_nrow
.cFuncs[["<-ncol"]] = C_element_ncol
.cFuncs[["<-sweep"]] = C_element_sweep
.cFuncs[["[<-"]] = C_assignment_symbols


.cFuncs[["return"]] = C_return
.cFuncs[["return_nocpy"]] = C_NULL
.cFuncs[["break"]] = C_break
.cFuncs[["next"]] = C_next
.cFuncs[["message"]] = C_message
.cFuncs[["setVersion"]] = C_setVersion
.cFuncs[["compiler.define"]] = C_compiler_define


#' @include RCParserFunc_Rlevel.R
.sizeFuncs=list()
.sizeFuncs[["["]]=R_subset_size
.sizeFuncs[["sweep"]]=R_sweep_size


 


general_size_function_list=c("+","-","*","/",">","<",">=","<=","==","!=","^",
                             "abs","abs_int","abs_float",
                             "floor", "ceiling","("
                             )
for(i in general_size_function_list){
  .sizeFuncs[[i]]=R_general_size
}
size_one_function_list=c("nrow","ncol","length")
for(i in size_one_function_list){
  .sizeFuncs[[i]]=R_size_returnOne
}

if(sum(!.elementFuncs%in%names(.sizeFuncs))!=0){
  funcName=.elementFuncs[!.elementFuncs%in%names(.sizeFuncs)]
  warning("The following element oprations does not have proper size function:\n",paste0(funcName,collapse = ", "))
}
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
#' a=Scalar(precision='double',constDef=FALSE)
#' 
#' @return a variable initialize with 0.
#' @export
Scalar <- function(precision = GPUVar$default_float, constDef = FALSE) {
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
#' @param location The physical memory location of the matrix, it can be either 'global' or 'local'. Do not use it if you don't know its meaning.
#' @aliases Matrix
#' @examples 
#' #Create a 10-by-10 matrix
#' A=Matrix(10,10)
#' @return a matrix initialize with 0.
#' @export
Matrix <- function(nrow = 1, ncol = 1, precision = GPUVar$default_float, 
    constDef = FALSE, shared = FALSE, location = "global") {
    return(matrix(NA, nrow, ncol))
}
# TODO
resize <- function(data, nrow, ncol) {
    return(matrix(data, nrow, ncol))
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
subRef <- function(variable, i = "", j = "") {
    if (length(i) == 1 && length(j) == 1 && i == "" && j == "") 
        return(variable[, , drop = FALSE])
    if (length(i) == 1 && i == "") 
        return(variable[, j, drop = FALSE])
    if (length(j) == 1 && j == "") 
        return(variable[i, , drop = FALSE])
    return(variable[i, j, drop = FALSE])
}
#' No copy method
#' 
#' Doing some opration without memory copy
#' 
#' @details 
#' `return_nocpy`: The usage of the `return_nocpy` is same as `return`. This feature is for openCL code only, 
#' if it is called in R, the function `return()` will be called instead
#' 
#' @param x an object
#' @return `return_nocpy`: No return value
#' @rdname no_copy_method
#' @aliases return_nocpy
#' @usage return_nocpy(x)
#' @examples 
#' x=matrix(0)
#' #return_nocpy(x)
#' @export return_nocpy
return_nocpy = return


#' @details 
#' `t_nocpy`: The function transposes `x` without allocating the memory. It only works for the openCL code, 
#' if it is called in R, the function `t()` will be called instead
#' 
#' @return `t_nocpy`: the transpose of `x`
#' @rdname no_copy_method
#' @aliases t_nocpy
#' @usage t_nocpy(x)
#' @examples 
#' #x=t_nocpy(x)
#' @export t_nocpy
t_nocpy=function(x){
  t(x)
}

#Insert variable information into the varInfo
compiler.addInfo<-function(varName,...){
  
}
compiler.addScalarInfo<-function(varName,precisionType,...){
  
}
#Change the variable property(Must defined)
compiler.setProperty<-function(varName,...){
  
}

#define the variable(s)
compiler.define<-function(varName,...){
  
}
#If the variable is in used, then define it.
compiler.promiseDefine<-function(precision,varName){
  #paste0(precision," ",varName;")
}
#The compiler will define the variable in the following way
#1.Find if there is any variable that can be used in the memory pool, use it
#2.If the memory pool does not have any available variable, define a new variable in the pool to use it.
compiler.defineInPool<-function(precision,varName,def){
  
}
#TODO: define a variable but not monitored by the memory pool


#If the variable is in used, then do the assignment
compiler.promiseAssign<-function(target,code){
  paste0(GPUVar$promiseAssgin,target,"--",code)
}

compiler.release<-function(varName){
  
}





