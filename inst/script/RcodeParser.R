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
test2<-function(){
  a[1]=b[10]
  for(i in 1:10){
    a=2
    a[1]=b[i]
    a=matrix(0,10,10)
    if(b==10){
      a[1]=b[i]
      d=a+b+d
    }
  }
  
  if(b==10){
    a=3
    a=100
    d=50
    d=a+b+d
  }
}

n=2
m=2
k=2
A=matrix(runif(n*m),n,m)
B=matrix(runif(n*m),m,k)
C=matrix(0,n,k)


test3<-function(ind,A,B,C){
  ind=ind-1
  j=floor(ind/nrow(C))
  i=ind-j*nrow(C)
  j=j+1
  i=i+1
  C[i,j]=0
  for(k in 1:ncol(A)){
    C[i,j]=C[i,j]+A[i,k]*B[k,j]
  }
  return(C[i,j])
}



test3<-function(ind,A,B,C){
  a=nrow(C)
  return(C[i,j])
}

res=sapply(1:length(C),test3,A,B,C)

C_res=matrix(res,n,k)

max(abs(C_res-A%*%B))

eval(parse(text="a=1"))

parsedExp=funcToExp(test3)$code
level1Exp=RRcompilerLevel1(parsedExp)
level2Exp=RRcompilerLevel2(level1Exp)
level3Exp=RRcompilerLevel3(level2Exp)
parms=list(ind=1,A=A,B=B,C=C)
profileMeta1=RProfilerLevel1(level3Exp,parms)
profileMeta2=RProfilerLevel2(profileMeta1)


GPUExp1=RCcompilerLevel1(profileMeta2)


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
