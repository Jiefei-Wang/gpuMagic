#' @include pkgFunc.R
#======================GPU address S5 class====================

gpuRefAddress=setRefClass("gpuRefAddress", fields = c("address","dim","type","device","isReady"))
gpuRefAddress$methods(
  initialize = function(data,type,repNum) {
    data=as.matrix(data)
    .self$dim=dim(data)
    .self$dim[2]=.self$dim[2]*repNum
    .self$type=type
    .self$device=getCurDeviceIndex()
    .self$isReady=TRUE
    .self$address=.gpuResourcesManager$upload(.self,data,repNum,type)
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
  finalize = function() {
      .gpuResourcesManager$releaseAddress(.self,.self$address)
  }
)
gpuRefAddress$methods(
  upload = function(data,type) {
    .gpuResourcesManager$releaseAddress(.self,.self$address)
    .self$initialize(data,type)
  }
)
gpuRefAddress$methods(
  download = function() {
    .gpuResourcesManager$download(.self$address,.self$dim,.self$type)
  }
)

gpuRefAddress$methods(
  switchDevice = function() {
    data=.gpuResourcesManager$download(.self$address,.self$dim,.self$type)
    .self$initialize(data,.self$type)
  }
)




