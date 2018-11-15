recompile_matrix<-function(varInfo,curExp){
  leftExp=curExp[[2]]
  leftExp_char=deparse(leftExp)
  rightExp=curExp[[3]]
  rightExp=standardise_call(rightExp)
  argNames=names(rightExp)
  data_ind=which(argNames=="data")
  nrow_ind=which(argNames=="nrow")
  ncol_ind=which(argNames=="ncol")
  if(length(nrow_ind)==0)
    rowNum=1
  else
    rowNum=deparse(rightExp[[nrow_ind]])
  if(length(ncol_ind)==0)
    colNum=1
  else
    colNum=deparse(rightExp[[ncol_ind]])
  data_value=deparse(rightExp[[data_ind]])
  curCode=paste0(leftExp_char,"=gMatrix(nrow=",rowNum,",ncol=",colNum,
                 ",precision=\"",gpuMagic.option$getDefaultFloat()
                 ,"\",loc=\"global\")")
  curCode=c(curCode,paste0("for(i in 1:",rowNum,"){"))
  curCode=c(curCode,paste0("for(j in 1:",colNum,"){"))
  curCode=c(curCode,paste0(leftExp_char,"[i,j]=",data_value))
  curCode=c(curCode,"}}")
  curCode=as.list(parse(text=paste0(curCode,collapse = "\n")))
  curCode
}




recompile_matrixMult<-function(varInfo,curExp){
  code=c()
  left_var=deparse(curExp[[2]])
  right_var1=deparse(curExp[[3]][[2]])
  right_var2=deparse(curExp[[3]][[3]])
  left_info=getVarInfo(varInfo,left_var)
  right_info1=getVarInfo(varInfo,right_var1)
  right_info2=getVarInfo(varInfo,right_var2)
  
  pvs=GPUVar$private_var_space
  ps=GPUVar$private_size
  code=paste0(pvs,"=gMatrix(nrow=1,ncol=",ps,",fixed=TRUE,location=\"local\")")
  code=c(code,
         paste0(left_var,"=gMatrix(nrow=",matrixNrow(right_var1),
                ",ncol=",matrixNcol(right_var2),",shared=",left_info$shared,
                ",location=",left_info$location,")"),
         paste0("for(",pvs,"_i1 in 1:nrow(",left_var,")){"),
         paste0("for(",pvs,"_i2 in 1:ncol(",left_var,")){"),
         paste0(left_var,"[",pvs,"_i1,",pvs,"_i2]=0"),
         "}",
         "}",
         paste0("for(",pvs,"_i1 in 1:",matrixNrow(right_var1),"){"),
         paste0(pvs,"_loopNum=ceiling(",matrixNcol(right_var1),"/",ps,")"),
         paste0(pvs,"_start=1"),
         paste0(pvs,"_end=1"),
         paste0(pvs,"_length=0"),
         paste0("for(",pvs,"_t in 1:",pvs,"_loopNum){"),
         paste0(pvs,"_start=",pvs,"_end"),
         paste0(pvs,"_end=",pvs,"_end+",ps),
         paste0("if(",pvs,"_end>",matrixNcol(right_var1),"){"),
         paste0(pvs,"_end=",matrixNcol(right_var1),"+1"),
         "}",
         paste0(pvs,"_length=",pvs,"_end-",pvs,"_start"),
         paste0("for(",pvs,"_t1 in 1:",pvs,"_length){"),
         paste0(pvs,"[",pvs,"_t1]=",matrixInd(right_var1,paste0(pvs,"_i1")
                                              ,paste0(pvs,"_start+",pvs,"_t1-1"))),
         "}",
         paste0("for(",pvs,"_j2 in 1:",matrixNcol(right_var2),"){"),
         paste0(pvs,"_tmp=0"),
         paste0("for(",pvs,"_j1 in 1:",pvs,"_length){"),
         paste0(pvs,"_tmp=",pvs,"_tmp+",pvs,"[",pvs,"_j1]*",
                matrixInd(right_var2,paste0(pvs,"_start+",pvs,"_j1-1"),paste0(pvs,"_j2"))),
         "}",
         paste0(matrixInd(left_var,paste0(pvs,"_i1"),paste0(pvs,"_j2")),
                "=",matrixInd(left_var,paste0(pvs,"_i1"),paste0(pvs,"_j2")),"+",
                pvs,"_tmp"),
         "}",
         "}",
         "}"
         )
  #code=paste0(code,collapse = "\n")
  code=parse(text=code)
  code
}

matrixInd<-function(var,i,j){
  paste0(var,"[",i,",",j,"]")
}
matrixNrow<-function(var){
   paste0("nrow(",var,")")
}
matrixNcol<-function(var){
  paste0("ncol(",var,")")
}
