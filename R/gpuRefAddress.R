#======================GPU address S5 class====================

gpuRefAddress=setRefClass("gpuRefAddress", fields = c("address","device"))
gpuRefAddress$methods(
  initialize = function() {
    .self$address=NULL
  }
)

gpuRefAddress$methods(
  getAddress = function() {
    .self$address
  }
)
gpuRefAddress$methods(
  setAddress = function(ad) {
    .self$address=ad
  }
)

gpuRefAddress$methods(
  finalize = function() {
    ad=.self$getAddress()
    if(!is.null(ad))
      .gpuResourcesManager$releaseAddress(.self$device,ad)
  }
)

gpuRefAddress$methods(
  gpuMalloc = function(device,len,type) {
    ad=.self$getAddress()
    if(!is.null(ad))
      .gpuResourcesManager$releaseAddress(.self$device,ad)
    .self$device=device
    .self$setAddress(.gpuResourcesManager$gpuMalloc(device,len,type))
  }
)

gpuRefAddress$methods(
  upload = function(device,data,type) {
    ad=.self$getAddress()
    if(!is.null(ad))
      .gpuResourcesManager$releaseAddress(.self$device,ad)
    .self$device=device
    .self$setAddress(.gpuResourcesManager$upload(device,data,type))
  }
)
gpuRefAddress$methods(
  download = function() {
    ad=.self$getAddress()
    .gpuResourcesManager$download(.self$device,ad)
  }
)

gpuRefAddress$methods(
  switchDevice = function(device) {
    stop("not supported yet!")
    #ad=.self$getAddress()
    #data=.gpuResourcesManager$download(.self$device,ad)
    #.self$upload(device,data,.self$type)
    #.self$device=device
  }
)






