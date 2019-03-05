get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

switch(get_os(),
       linux=cat("-lOpenCL"),
       osx=cat("-framework OpenCL"),
       windows=cat(paste0(Sys.getenv("SystemRoot")[1],"/System32/OpenCL.dll")),
       stop("Unable to determine the OS!")
       )
