
#The return size still has bug
testFunc<-function(ind,A,B){
  C=A+B
  return.nocpy(C)
}
n=3
m=4
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),n,m)
parms=matchParms(1:m,list(A,B),testFunc)
opt=gpuSapply.getOption()
macroParms=NULL
codeMetaInfo=list()
codeMetaInfo$Exp=funcToExp(testFunc)$code
codeMetaInfo$parms=parms
codeMetaInfo$macroParms=macroParms
codeMetaInfo0=codePreprocessing(codeMetaInfo)
codeMetaInfo1=RParser1(codeMetaInfo0)
codeMetaInfo2=RParser2(codeMetaInfo1)
profileMeta1=RProfile1(codeMetaInfo2)
profileMeta2=RProfile2(profileMeta1)
GPUExp1=RCcompilerLevel1(profileMeta2)
GPUExp2=RCcompilerLevel2(GPUExp1)
GPUcode=completeProfileTbl(GPUExp2)
GPUcode1=completeGPUcode(GPUcode)
GPUcode2=fillGPUdata(GPUcode1,.options=opt,.device=getFirstSelectedDevice())

printVarInfo(profileMeta2$varInfo)


Exp=quote(gpu_loop_data[opencl_tmp_1])
R_expression_sub(varInfo,Exp,1,1)

C_subset(varInfo,Exp)
renameVarInCode(codeMetaInfo0$Exp,1,"break","break")


substitute(codeMetaInfo0$Exp,list("break"="opencl_break","next"="opencl_continue"))
do.call('substitute', list(code[[i]], renameList))