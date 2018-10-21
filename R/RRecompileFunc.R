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
  curCode=deparse(curExp)
  curCode=c(curCode,paste0("for(i in 1:",rowNum,"){"))
  curCode=c(curCode,paste0("for(j in 1:",colNum,"){"))
  curCode=c(curCode,paste0(leftExp_char,"[i,j]=",data_value))
  curCode=c(curCode,"}}")
  curCode=as.list(parse(text=paste0(curCode,collapse = "\n")))
  curCode
}



.recompileFuncs=list()
.recompileFuncs$matrix=recompile_matrix