#' @include pkgFunc.R
#======================GPU address S5 class====================

gpuRefAddress=setRefClass("gpuRefAddress", fields = c("address","dim","type","device","isReady","isReleased"))
gpuRefAddress$methods(
  initialize = function(data,type) {
    data=as.matrix(data)
    .self$dim=dim(data)
    .self$type=type
    .self$device=getCurDeviceIndex()
    .self$isReady=TRUE
    .self$isReleased=FALSE
    .self$address=.gpuResourcesManager$upload(self,data,type)
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
    if(isReleased) stop("The data has been released!")
    .gpuResourcesManager$getAddress(.self$address)
  }
)
gpuRefAddress$methods(
  finalize = function() {
    if(!isReleased){
      .gpuResourcesManager$releaseAddress(.self$address)
      isReleased=TRUE
    }
  }
)
gpuRefAddress$methods(
  upload = function(data,type) {
    .gpuResourcesManager$releaseAddress(.self$address)
    .self$initialize(data,type)
  }
)
gpuRefAddress$methods(
  download = function() {
    .gpuResourcesManager$download(.self$address,.self$dim,.self$type)
  }
)
gpuRefAddress$methods(
  getReadyStatus = function() {
    .self$isReady
  }
)
gpuRefAddress$methods(
  setReadyStatus = function(status) {
    .self$isReady=status
  }
)
gpuRefAddress$methods(
  getReleaseStatus = function() {
    .self$isReleased
  }
)
gpuRefAddress$methods(
  switchDevice = function() {
    data=.gpuResourcesManager$download(.self$address,.self$dim,.self$type)
    .self$initialize(data,.self$type)
  }
)




