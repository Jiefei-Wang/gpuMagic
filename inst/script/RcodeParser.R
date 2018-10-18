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
  for(i in 1:g(10)){
    a=2
    a[1]=b[i]
    a=matrix(0,10,10)
    if(b==f(10)){
      a[1]=b[i]
      c=a+b+c
    }
  }
  
  if(b==f(10)){
    a=3
    a=100
    c=a+b+c
  }
}


eval(parse(text="a=1"))

parsedExp=funcToExp(test2)$code
level1Exp=RRcompilerLevel1(parsedExp)
level2Exp=RRcompilerLevel2(level1Exp)
level3Exp=RRcompilerLevel3(level2Exp)
parms=list(a=matrix(c(1,2,3,4),2,4),b=10,d=c(3,2,1))
compileInfo1=RCcompilerLevel1(level3Exp,parms)
compileInfo2=RCcompilerLevel2(compileInfo1)
compileInfo3=RCcompilerLevel3(compileInfo2)


printExp(parsedExp)
printExp(level1Exp$Exp)
printExp(level2Exp$Exp)
printExp(level3Exp$Exp)
printExp(compileInfo1$code)
compileInfo3$gpu_code


level1Exp$code
b

for(i in 1:length(b))
  print(b[[i]])

compileInfo1$varInfo
compileInfo2$varInfo
