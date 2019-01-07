#======================GPU address S5 class====================

gpuRefAddress=setRefClass("gpuRefAddress", fields = c("address","length","type","device"))
gpuRefAddress$methods(
  initialize = function() {
    .self$address=NULL
  }
)

gpuRefAddress$methods(
  getType = function() {
    .self$type
  }
)
gpuRefAddress$methods(
  getDevice = function() {
    .self$device
  }
)
gpuRefAddress$methods(
  getAddress = function() {
    if(!is.null(.self$address))
      return(.self$address)
    else
      stop("The GPU resources does not exist")
  }
)

gpuRefAddress$methods(
  finalize = function() {
    if(!is.null(.self$address))
      .gpuResourcesManager$releaseAddress(.self$device,.self$address)
  }
)

gpuRefAddress$methods(
  gpuMalloc = function(device,len,type) {
    if(!is.null(.self$address))
      .gpuResourcesManager$releaseAddress(.self$device,.self$address)
    .self$length=len
    .self$type=type
    .self$device=device
    .self$address=.gpuResourcesManager$gpuMalloc(device,len,type)
  }
)

gpuRefAddress$methods(
  upload = function(device,data,type) {
    if(!is.null(.self$address))
      .gpuResourcesManager$releaseAddress(.self$device,.self$address)
    data=as.matrix(data)
    .self$length=length(data)
    .self$type=type
    .self$device=device
    .self$address=.gpuResourcesManager$upload(device,data,type)
  }
)
gpuRefAddress$methods(
  download = function() {
    .gpuResourcesManager$download(.self$device,.self$address,.self$length,.self$type)
  }
)

gpuRefAddress$methods(
  switchDevice = function(device) {
    data=.gpuResourcesManager$download(.self$device,.self$address,.self$length,.self$type)
    upload(device,data,.self$type)
    .self$device=device
  }
)




