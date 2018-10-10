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



tmp=a[[3]][[2]][[3]][[2]]

tmpInd=1
parsedExp=a[[3]][[2]][[3]][[2]]
createTmp<-function(tmpInd,code,parsedExp){
  res=getTmpName(tmpInd)
  tmpInd=res[[1]]
  tmpName=res[[2]]
  varList=rep(0,length(parsedExp)-1)
  for(i in seq(length(parsedExp)-1)){
    if(length(parsedExp[[i+1]])!=1){
      res=createTmp(tmpInd,code,parsedExp[[i+1]])
      tmpInd=res[[1]]
      tmpName=res[[2]]
      code=res[[3]]
      varList[i]=tmpName
    }else{
      varList[i]=as.character(parsedExp[[i+1]])
    }
  }
  code=operatorDispatch(code,parsedExp[[1]],varList)
  return(list(tmpInd,tmpName,code))
}


getTmpName<-function(tmpInd){
  return(list(tmpInd+1,paste0("_tmp_",tmpInd)))
}

operatorDispatch<-function(code,operator,varList){
  switch(operator,
         "+"=level1_parse_add(code,varList)
         )
}


level1_parse_add<-function(){}


