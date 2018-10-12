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
  d = a[10:11] + b+f() + 10
  c[1:10]=11:20
}

eval(parse(text="a=1"))

parsedExp=funcToExp(test)
level1Exp=compilerLevel1(parsedExp)
level2Exp=compilerLevel2(level1Exp)
parsedExp1=level2Exp$code
parsedExp1


compilerLevel3<-function(level2Exp,...){
  
  tmpInd=level2Exp$tmpInd
  parsedExp=level2Exp$code
  varInfo=list()
  varInfo$profile=getEmpyTableLevel3(0)
  varInfo$rename=hash()
  varInfo$address=data.frame(storage=0,sizeList=0,typeList=0)
  #Load the provided matrix information
  varInfo=parInfo(varInfo,...)
  
  for(curExp in parsedExp)
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      if(length(leftExp)==1){
        
      }else{
        
      }
    }
}
varInfo=parInfo(varInfo,a=c(1,2,3),b=matrix(c(4,3,5,22),2,2),d=matrix(c(2,2,2,2),10,11))


createNewVarLevel3<-function(varInfo,target,src){
  if(is.symbol(src)){
    ind=findVarIndLevel3(varInfo,src)
    info=varInfo$profile[ind,,drop=F]
  }else{
    info=getEmpyTableLevel3(1)
    info=getFuncReturnInfoLevel3(varInfo,src)
  }
  info$var=as.character(target)
  if(is.na(info$size1)||is.na(info$size2))
    info$kernel_var=NA
  else{
    info$kernel_var=paste0(info$var,"+",varInfo$address$storage)
    varInfo$address$storage=varInfo$address$storage+info$size1*info$size2*getTypeSize(info$type)
  }
  varInfo$profile=rbind(varInfo$profile,info)
  return()
}
getFuncReturnInfoLevel3<-function(varInfo,src){
  info=getEmpyTableLevel3(1)
  if(src[[1]]=="+"){
    ind1=findVarIndLevel3(varInfo,src[[2]])
    ind2=findVarIndLevel3(varInfo,src[[3]])
    info1=varInfo$profile[ind1,,drop=F]
    info2=varInfo$profile[ind2,,drop=F]
    if(info1$size1!=info2$size1&&info1$size2!=info2$size2){
      if(info1$size1+info1$size2!=2&&info2$size1+info2$size2!=2)
        stop(paste0("Unsupported add function call: ",info1$var,"+",info2$var))
    }
    info$size1=max(info1$size1,info2$size1)
    info$size2=max(info1$size2,info2$size2)
    if(getTypeSize(info1$type)>getTypeSize(info2$type))
      info$type=info1$type
    else
      info$type=info2$type
    return(info)
  }
  if(src[[1]]=="["){
    ind=findVarIndLevel3(varInfo,src[[2]])
    tmp_info=varInfo$profile[ind,,drop=F]
    info$size1=tmp_info$size1
    info$size2=tmp_info$size2
    info$type=tmp_info$type
    
    if(src[[3]]!=""){
      ind1=findVarIndLevel3(varInfo,src[[3]])
      info1=varInfo$profile[ind1,,drop=F]
      info$size1=info1$size1
    }
    if(length(src)==4){
      if(src[[4]]!=""){
        ind2=findVarIndLevel3(varInfo,src[[4]])
        info2=varInfo$profile[ind2,,drop=F]
        info$size2=info2$size1
      }
    }else{
      info$size2=1
    }
    return(info)
  }
  
  info
}


findVarIndLevel3<-function(varInfo,var,willStop=TRUE){
  if(has.key(as.character(var),varInfo$rename))
    var=varInfo$rename[[as.character(var)]]
  else
    var=as.character(var)
  ind=which(varInfo$profile$var==var) 
  if(length(ind)==0&&willStop)
    stop(paste0("Undefined variable: ",var))
  ind
}

#Add the parameter info
parInfo<-function(varInfo,...){
  varInfo_name=names(varInfo$profile)
  parms=list(...)
  parmsName=names(parms)
  if(sum(parmsName=="")+is.null(parmsName)>0) stop("All the variable names should be supplied in the function call\n eg: matrixA=A")
  for(i in seq_len(length(parms))){
    if(class(parms[[i]])!="gpuMatrix"){
      curDim=dim(as.matrix(parms[[i]]))
      type=T_F64
      info=getEmpyTableLevel3(1)
      info[1:5]=data.frame(parmsName[i],paste0("gpuMagic_var_",i),curDim[1],curDim[2],type)
    }else{
      curDim=dim(parms[[i]])
      type=getTypeNum(.type(parms[[i]]))
      info=getEmpyTableLevel3(1)
      info[1:5]=data.frame(parmsName[i],paste0("gpuMagic_var_",i),curDim[1],curDim[2],type)
    }
    varInfo$profile=rbind(varInfo$profile,info)
  }
  names(varInfo$profile)=varInfo_name
  varInfo
}
getEmpyTableLevel3<-function(rowNum=0){
  tbl=as.data.frame(matrix(NA,ncol = 8, nrow = rowNum))
  names(tbl)=c("var","kernel_var", "size1","size2",
               "type","content1","content2","content3")
  tbl
}


#Level2 compiler
#Functions:
#1.For "=" sign: If the left side symbol is subsetted, 
#the right side symbol should be a symbol,
#if not, create a temporary function to replace it
compilerLevel2<-function(level1Exp){
  tmpInd=level1Exp$tmpInd
  parsedExp=level1Exp$code
  code=c()
  for(curExp in parsedExp){
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      if(length(leftExp)!=1&length(rightExp)!=1)
      {
        res=createNewVarLevel1(tmpInd,code,rightExp)
        tmpInd=res$tmpInd
        tmpName=res$TmpName
        code=res$code
        curExp[[3]]=as.symbol(tmpName)
      }
    }
    code=c(code,curExp)
  }
  return(list(tmpInd=tmpInd,code=code))
}


#Level 1 compiler
#Functions:
#1.simplify the R code, each line should only have one function call,
#If not, a temporary variable will be created to replace it.
compilerLevel1<-function(parsedExp){
  code=c()
  tmpInd=1
  for(curExp in parsedExp){
    if(curExp[[1]]=="="){
      for(j in 2:3){
      oneSideExp=curExp[[j]]
      if(length(oneSideExp)>=2){
        for(i in seq(2,length(oneSideExp))){
          if(length(oneSideExp[[i]])!=1){
            res=createNewVarLevel1(tmpInd,code,oneSideExp[[i]])
            tmpInd=res$tmpInd
            tmpName=res$TmpName
            code=res$code
            curExp[[j]][[i]]=as.symbol(tmpName)
          }
        }
      }
      }
      code=c(code,curExp)
    }
  }
  return(list(tmpInd=tmpInd,code=code))
}


createNewVarLevel1<-function(tmpInd,code,parsedExp){
  res=getTmpName(tmpInd)
  tmpInd=res$tmpInd
  curTmpName=res$tmpName
  for(i in seq(2,length(parsedExp))){
    #If the element is a function call
    if(length(parsedExp[[i]])!=1){
      res=createNewVarLevel1(tmpInd,code,parsedExp[[i]])
      tmpInd=res$tmpInd
      tmpName=res$TmpName
      code=res$code
      parsedExp[[i]]=as.symbol(tmpName)
    }
  }
  code=c(
    code,
    parse(text=paste0(curTmpName,"=",deparse(parsedExp,backtick=F)))
    )
  return(list(tmpInd=tmpInd,TmpName=curTmpName,code=code))
}


getTmpName<-function(tmpInd){
  return(list(tmpInd=tmpInd+1,tmpName=paste0("opencl_tmp_",tmpInd)))
}

#convert a function to an expression
funcToExp<-function(f){
  charExp=deparse(f)
  charExp=charExp[c(-1,-2,-length(charExp))]
  parsedExp=parse(text=charExp)
  parsedExp
}



