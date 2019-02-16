getThreadNumber<-function(options){
  if (options$localThreadNum == "auto") {
    localThreadNum = as.numeric(gpuMagic.getOptions("default.thread.num")) 
  } else {
    localThreadNum = options$localThreadNum
  }
  return(localThreadNum)
}
# great common divisor
gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}
