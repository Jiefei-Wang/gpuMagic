#' @include pkgFunc.R
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
    .gpuResourcesManager$getAddress(.self$address)
  }
)

gpuRefAddress$methods(
  finalize = function() {
      .gpuResourcesManager$releaseAddress(.self$address)
  }
)

gpuRefAddress$methods(
  gpuMalloc = function(len,type) {
    if(!is.null(.self$address))
      .gpuResourcesManager$releaseAddress(.self,.self$address)
    .self$length=len
    .self$type=type
    .self$device=getCurDeviceIndex()
    .self$address=.gpuResourcesManager$gpuMalloc(len,type)
  }
)

gpuRefAddress$methods(
  upload = function(data,type) {
    if(!is.null(.self$address))
      .gpuResourcesManager$releaseAddress(.self$address)
    data=as.matrix(data)
    .self$length=length(data)
    .self$type=type
    .self$device=getCurDeviceIndex()
    .self$address=.gpuResourcesManager$upload(data,type)
  }
)
gpuRefAddress$methods(
  download = function() {
    .gpuResourcesManager$download(.self$address,.self$length,.self$type)
  }
)

gpuRefAddress$methods(
  switchDevice = function() {
    data=.gpuResourcesManager$download(.self$address,.self$length,.self$type)
    upload(data,.self$type)
  }
)




