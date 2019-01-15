a=gpuEmptMatrix()
a@data="aa"
test<-function(){
b=.Call("test")
return(b)
}
test()
attributes(a)



x <- cbind(a = 1:3, pi = pi) # simple matrix with dimnames
attributes(x)$test=.Call("test")
e=new.env()
e$a=.Call("test")

n=100
data=runif(n)
a=gpuMatrix(data)
d=.getAddress(a)
.gpuResourcesManager$internalVars$addressSizeList
.gpuResourcesManager$releaseAll()

.gpuResourcesManager$getGPUusage()

type="double"
a=.Call(
  "upload",0L,0L,data,
  as.double(length(data)),getTypeNum(type)
)
res=.Call("download",e$a)
