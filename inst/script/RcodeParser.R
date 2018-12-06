
#The return size still has bug
testFunc<-function(col_ind,A,B){
  A[1,]=10+5+44
  
  C=A[1,]%*%B*O
}
n=3
m=4
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),n,m)
parms=matchParms(1:m,list(A,B),testFunc)
macroParms=c("A")
codeMetaInfo=list()
codeMetaInfo$Exp=funcToExp(testFunc)$code
codeMetaInfo$parms=parms
codeMetaInfo$macroParms=macroParms
codeMetaInfo0=codePreprocessing(codeMetaInfo)
codeMetaInfo1=RParser1(codeMetaInfo0)
codeMetaInfo2=RParser2(codeMetaInfo1)
profileMeta1=RProfile1(codeMetaInfo2)
profileMeta2=RProfile2(profileMeta1)
profileMeta3=RRecompiler(profileMeta2)
GPUExp1=RCcompilerLevel1(profileMeta3)
GPUExp2=RCcompilerLevel2(GPUExp1)
GPUcode=completeProfileTbl(GPUExp2)
GPUcode1=completeGPUcode(GPUcode)


Exp=quote(gpu_loop_data[opencl_tmp_1])
R_expression_sub(varInfo,Exp,1,1)

C_subset(varInfo,Exp)
renameVarInCode(codeMetaInfo0$Exp,1,"break","break")


substitute(codeMetaInfo0$Exp,list("break"="opencl_break","next"="opencl_continue"))
do.call('substitute', list(code[[i]], renameList))