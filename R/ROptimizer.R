ROptimizer1 <- function(profileMeta2) {
  previousExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  for(i in seq_len(previousExp)){
    curExp=previousExp[[i]]
    vars=unique(extractVars(curExp))
    varsInUsed=variableInUsed(varInfo,vars)
  }
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
            root=c(root,curInfo$var)
          }
      }
    }else{
      root=c(root,curvar)
    }
  }
  root
}