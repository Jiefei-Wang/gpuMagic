
#Profile the parameters only
RProfilerLevel1<-function(level3Exp){
  tmpInd=level3Exp$tmpInd
  parsedExp=level3Exp$Exp
  
  #The loop variable needs some special treatment, I rename it so that it would not be confused with the total looped variable
  workerData=names(level3Exp$parms)[1]
  tmp_parms=c(level3Exp$parms)
  names(tmp_parms)[1]=GPUVar$gpu_worker_data
  
  varInfo=profileVar(tmp_parms)
  loopVar=getEmpyTable(1,type=T_scale)
  loopVar$var=workerData
  varInfo$profile=rbind(varInfo$profile,loopVar)
  varInfo$varTable[[workerData]]=nrow(varInfo$profile)
  
  profileMeta1=level3Exp
  profileMeta1$tmpInd=tmpInd
  profileMeta1$Exp=parsedExp
  profileMeta1$varInfo=varInfo
  profileMeta1$workerData=workerData
  
  
  return(profileMeta1)
}




#Profile the variables in the code
RProfilerLevel2<-function(profileMeta1){
  tmpInd=profileMeta1$tmpInd
  parsedExp=profileMeta1$Exp
  varInfo=profileMeta1$varInfo
  
  
  for(i in 1:length(parsedExp)){
    #if(i==3)stop()
    curExp=parsedExp[[i]]
    #message(curExp)
    if(curExp=="{"){
      next
    }
    if(curExp[[1]]=="="){
      leftExp=curExp[[2]]
      rightExp=curExp[[3]]
      #Extract the variable in the left expression
      if(is.call(leftExp)){
        if(leftExp[[1]]!="[")
          stop("Unrecognized code",deparse(curExp))
        
      }else{
        var_char=deparse(leftExp)
        #Check if the variable is in the variable table, if not, profile the variable
        if(!has.key(var_char,varInfo$varTable)){
          ExpProfile=getExpInfo(varInfo,rightExp)
          ExpProfile$var=var_char
          
          varInfo$profile=rbind(varInfo$profile,ExpProfile)
          varInfo$varTable[[var_char]]=nrow(varInfo$profile)
        }
      }
      next
    }
    
    if(curExp[[1]]=="return"){
      ExpProfile=profile_return(varInfo,curExp)
      varInfo$returnInfo=ExpProfile
      next
    }
    #For loop
    if(curExp[[1]]=="for"){
      var_char=deparse(curExp[[2]])
      ExpProfile=getEmpyTable(1,type = T_scale)
      ExpProfile$var=var_char
      ExpProfile$initialization="N"
      varInfo$profile=rbind(varInfo$profile,ExpProfile)
      varInfo$varTable[[var_char]]=nrow(varInfo$profile)
      res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[4]],varInfo=varInfo))
      tmpInd=res$tmpInd
      varInfo=res$varInfo
      next
    }
    if(curExp[[1]]=="if"){
      res=RProfilerLevel2(list(tmpInd=tmpInd,Exp=curExp[[3]],varInfo=varInfo))
      tmpInd=res$tmpInd
      varInfo=res$varInfo
      next
    }
  }
  
  
  profileMeta2=profileMeta1
  profileMeta2$tmpInd=tmpInd
  profileMeta2$Exp=parsedExp
  profileMeta2$varInfo=varInfo
  
  
  return(profileMeta2)
}


















