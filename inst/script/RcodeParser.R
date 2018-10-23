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
  a=matrix(0,10,2)
}

test2<-function(){
  
  a=1
  a=2
  ca=matrix(0,1,10)
  ca=1
  #b=a
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
codeMetaInfo$Exp=funcToExp(test3)$code
codeMetaInfo$parms=parms
codeMetaInfo$staticParms=NULL
codeMetaInfo0=codePreprocessing(codeMetaInfo)
codeMetaInfo1=RParser1(codeMetaInfo0)
codeMetaInfo2=RParser2(codeMetaInfo1)
profileMeta1=RProfile1(codeMetaInfo2)
profileMeta2=RProfile2(profileMeta1)
profileMeta3=RRecompiler(profileMeta2)
GPUExp1=RCcompilerLevel1(profileMeta3)







