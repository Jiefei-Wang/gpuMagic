#' @importFrom pryr standardise_call




#Expand some codes if necessary
RRecompiler<-function(profileMeta2){
  tmpMeta=profileMeta2$tmpMeta
  parsedExp=profileMeta2$Exp
  varInfo=profileMeta2$varInfo
  profile=varInfo$profile
  for(i in 1:length(parsedExp)){
    curExp=parsedExp[[i]]
    #message(curExp)
    if(curExp=="{"){
      next
    }
    FuncName=deparse(curExp[[1]])
    
    if(FuncName=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      #Extract the variable in the left expression
      if(is.call(leftExp)){
        if(leftExp[[1]]!="[")
          stop("Unrecognized code",deparse(curExp))
      }else{
        var_char=deparse(leftExp)
        left_info=getVarInfo(varInfo,var_char)
        #Check if the variable is a matrix, if it is, it may need some expand
        if(left_info$dataType==T_matrix){
          if(is.call(rightExp)){
            rightFunc=deparse(rightExp[[1]])
            if(!is.null(.recompileFuncs[[rightFunc]])){
             curCode=.recompileFuncs[[rightFunc]](varInfo,curExp)
             parsedExp=insertAndReplaceCode(i,parsedExp,curCode)
            }else{
              stop("Unsupported code: ",deparse(curExp))
            }
          }
        }
      }
    }
    
    if(!is.null(.recompileFuncs[[FuncName]])){
      curCode=.recompileFuncs[[rightFunc]](varInfo,curExp)
      parsedExp=insertAndReplaceCode(i,parsedExp,curCode)
    }
    # 
    # #For loop
    # if(FuncName=="for"){
    #   var_char=deparse(curExp[[2]])
    #   ExpProfile=getEmpyTable(1,type = T_scale)
    #   ExpProfile$var=var_char
    #   ExpProfile$initialization="N"
    #   varInfo$profile=rbind(varInfo$profile,ExpProfile)
    #   varInfo$varTable[[var_char]]=nrow(varInfo$profile)
    #   res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[4]],varInfo=varInfo))
    #   tmpInd=res$tmpInd
    #   varInfo=res$varInfo
    # }
    # if(curExp[[1]]=="if"){
    #   res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[3]],varInfo=varInfo))
    #   tmpInd=res$tmpInd
    #   varInfo=res$varInfo
    # }
  }
  
  codeMetaInfo=list()
  codeMetaInfo$Exp=parsedExp
  codeMetaInfo$parms=profileMeta2$parms
  codeMetaInfo1=RParser1(codeMetaInfo,insertPreserved = F)
  codeMetaInfo2=RParser2(codeMetaInfo1)
  codeMetaInfo3=RParser3(codeMetaInfo2)
  profileMeta1=RProfiler1(codeMetaInfo3)
  profileMeta2=RProfiler2(profileMeta1)
  
  
  profileMeta2
}


insertAndReplaceCode<-function(i,parsedExp,newExp){
  if(i!=1)
    parsedExp_previous=c(parsedExp[1:(i-1)])
  else
    parsedExp_previous=c()
  if(i!=length(parsedExp))
    parsedExp_post=c(parsedExp[(i+1):length(parsedExp)])
  else
    parsedExp_post=c()
  parsedExp=c(parsedExp_previous,newExp,parsedExp_post)
  parsedExp
}

