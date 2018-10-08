
#dyn.load(.parms$getLibPath())
#dyn.unload(.parms$getLibPath())
#detach("package:openSparse",unload = T)



mydata=1:10
mydata1=gpuMatrix(mydata,1)
mydata1@data=0
mydata1=download(mydata1)


convertDataType(mydata,T_F64)
.gpuResourcesManager$getGPUusage()



src="
__kernel
void   
vector_add(autoType1 A, autoType2 B, autoType3 C) {
	 // Get the index of the current element to be processed
	int i = get_global_id(0);

	// Do the operation
	C[i] = A[i] + B[i];
}

__kernel
void 
vector_delete(autoType1 A, autoType2 B, autoType3 C) {
	 // Get the index of the current element to be processed
	int i = get_global_id(0);

	// Do the operation
	C[i] = A[i] + B[i];
}
"

cleanCode<-function(src){
  src=gsub("//.*?\n","",src)
  src=gsub("\t","",src)
  src=gsub("\n+"," ",src)
  
  src
}

src=cleanCode(src)

funcName=c()
parmNum=c()
res=gregexpr("__kernel void .+?[)]",src)
ind=res[[1]]
len=attr(ind,"match.length")
if(ind[1]!=-1){
for(i in 1:length(ind)){
tmp=substr(src,ind[i],len[i])
#Capture function name
func=gsub("[(].*","",tmp)
func=gsub(" ","",func)
funcName=c(funcName,substr(func,13,nchar(func)))
#Capture function parameter
parmNum=c(parmNum,lengths(gregexpr(",",tmp)))
}
}
