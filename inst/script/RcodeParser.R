library(pryr)

A=1
B=1
a=quote(function(a,b){
  a=a+b+10
  return(a)
})
a[[2]]
ast(
  function(a,b){
    a=a+b+10
    return(a)
})


a[[3]][[2]][[3]][[2]]

length(a[[3]][[2]][[3]])

substitute(a[[3]][[2]][[3]],list(a[[3]][[2]][[3]][[2]]=tmp))


test<-function(...){
  parms=list(...)
  parmsName=names(parms)
  message(parmsName)
  message(sum(parmsName=="")+is.null(parmsName))
}

test2<-function(){
  a=10
  exists("b",inherits = FALSE)
}
test1(test)



tmp=a[[3]][[2]][[3]][[2]]

tmpInd=1
ast(quote(a = a[10:11] + b+f() + 10))

test<-function(){
  a=10
  a=f()
  a[1]=1
  a[1,]=1
  a[,2]=2
  a[1,2]=3
  #A
  d = a[10:11] + b+f() + 10
  c[1:10]=11:20
}
test<-function(){
  a=matrix(0,10,10)
  b=ncol(a)
  c=nrow(a)
  d=a[12]
  e=length(a)
  
}

test1<-function(a,b){
  b=b+a[1]+a[2,f(b)]
  e=a
}

n=10
m=5
k=2
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)
C=matrix(0,n,k)
parms=list(ind=1:(n*k),A=A,B=B)

test3<-function(ind,A,B){
  ind=ind-1
  j=floor(ind/nrow(A))
  i=ind-j*nrow(A)
  j=j+1
  i=i+1
  C=0
  for(k in 1:ncol(A)){
    C=C+A[i,k]*B[k,j]
  }
  return(C)
}

test1<-function(){
  a=A
  return(a)
}

test2<-function(){
  
  a=1
  a=2
  b=a
  for(i in 1:10){
    b=10
    for(j in 1:nrow(a)){
      a=10
      b=10
    }
    a=10
    a1=a
    
  }
  
}



test3<-function(ind,A,B){
  ind=ind-1
  j=floor(ind/nrow(A))
  i=ind-j*nrow(A)
  j=j+1
  i=i+1
  C=0
  for(k in 1:ncol(A)){
    C=C+A[i,k]*B[k,j]
  }
  return(C)
}
n=10
A=runif(n)
B=runif(n)
parms=list(ind=1:(n*n),A=A,B=B)
codeMetaInfo=list()
codeMetaInfo$Exp=funcToExp(test4)$code
codeMetaInfo$parms=parms
codeMetaInfo1=RParser1(codeMetaInfo)
codeMetaInfo2=RParser2(codeMetaInfo1)
codeMetaInfo3=RParser3(codeMetaInfo2)
profileMeta1=RProfiler1(codeMetaInfo3)
profileMeta2=RProfiler2(profileMeta1)
profileMeta3=RRecompiler(profileMeta2)

GPUExp1=RCcompilerLevel1(profileMeta3)



res=sapply(1:(n*k),test3,A,B)
C_res=matrix(res,n,k)
max(abs(C_res-A%*%B))


n=1
A=runif(n)
B=runif(n)
parms=list(ind=1:(n*n),A=A,B=B)
test3<-function(ind,A,B){
  e=A[ind]+B[ind]
  return(e)
}

parsedExp=funcToExp(test2)$code
level1Exp=RRcompilerLevel1(parsedExp)
level2Exp=RRcompilerLevel2(level1Exp)
#parms=list(A=0)
level3Exp=RRcompilerLevel3(level2Exp,parms)
profileMeta1=RProfilerLevel1(level3Exp)
profileMeta2=RProfilerLevel2(profileMeta1)
profileMeta3=RRecompiler(profileMeta2)

GPUExp1=RCcompilerLevel1(profileMeta3)


printExp(parsedExp)
printExp(level1Exp$Exp)
printExp(level2Exp$Exp)
printExp(level3Exp$Exp)
printExp(GPUExp1$gpu_code)
cat(paste(compileInfo3$gpu_code, collapse = '\n'))


level1Exp$code
b

for(i in 1:length(b))
  print(b[[i]])

compileInfo1$varInfo
compileInfo2$varInfo
