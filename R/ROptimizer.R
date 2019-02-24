ROptimizer1 <- function(profileMeta2) {
  previousExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  #a vector of 2: start,end
  varUsedInfo=list()
  for(i in seq_along(previousExp)){
    curExp=previousExp[[i]]
    vars=unique(extractVars(curExp))
    varsInUsed=unique(variableInUsed(varInfo,vars))
    for(curVar in varsInUsed){
      if(is.null(varUsedInfo[[curVar]])){
        varUsedInfo[[curVar]]=c(i,i)
      }else{
        varUsedInfo[[curVar]][2]=i
      }
    }
  }
  
  varUsedInfo[[GPUVar$gpu_global_id]]=NULL
  varUsedInfoTbl=do.call("rbind", varUsedInfo)
  
  vars=row.names(varUsedInfoTbl)
  
  Exp=c()
  for(i in rev(seq_along(previousExp))){
    curReleasedVarInd=which(varUsedInfoTbl[,2]==i)
    curNewVarInd=which(varUsedInfoTbl[,1]==i)
    curReleasedVar=names(curReleasedVarInd)
    curNewVar=names(curNewVarInd)
    if(length(curNewVar)>0){
      defCode=paste0("compiler.define(",paste0(curNewVar,collapse = ","),")")
      defCode=parse(text=defCode)[[1]]
    }else{
      defCode=NULL
    }
    if(length(curReleasedVar)>0){
      releaseCode=paste0("compiler.release(",paste0(curReleasedVar,collapse = ","),")")
      releaseCode=parse(text=releaseCode)[[1]]
    }else{
      releaseCode=NULL
    }
    Exp=c(Exp,
          releaseCode,
          previousExp[[i]],
          defCode
    )
  }
  optMeta1_0=profileMeta2
  optMeta1_0$Exp=rev(Exp)
  optMeta1_1=RProfile1(optMeta1_0)
  optMeta1_2=RProfile2(optMeta1_1)
  optMeta1_2
}



variableInUsed<-function(varInfo,vars){
  res=lapply(vars,findRootVar,varInfo=varInfo)
  unlist(res)
}

findRootVar<-function(varInfo,Exp){
  Exp=toExpression(Exp)
  vars=extractVars(Exp)
  root=c()
  for(i in vars){
    if(is.call(i)){
      rootc(root,findRootVar(varInfo,i))
      next
    }
    curvar=toCharacter(i)
    if(hasVar(varInfo,curvar)){
      curInfo=getVarInfo(varInfo,curvar)
      if(curInfo$redirect!="NA"){
        root=c(root,findRootVar(varInfo,curInfo$redirect))
      }else{
          if(curInfo$specialType=="ref"){
            root=c(root,findRootVar(varInfo,curInfo$specialContent))
          }else{
            if(!curInfo$shared){
              root=c(root,curInfo$var)
            }
         }
      }
    }
  }
  root
}