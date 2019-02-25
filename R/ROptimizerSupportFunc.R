#######################Optimizer 1#############################
#Find which variables are in used
#Can handle redirect and subref
variableInUsed<-function(varInfo,vars){
  res=lapply(vars,findRootVar,varInfo=varInfo)
  unlist(res)
}
#Find the in used variables for the current expression
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
#######################Optimizer 2#############################