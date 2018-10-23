opt_workerNumber<-function(code,workerNum){
  targetCode="unsigned long gpu_global_id=get_global_id(0);\n"
  insertedCode=paste0("if(gpu_global_id<",workerNum,"){\n")
  code=sub(targetCode,paste0(targetCode,insertedCode),code,fixed = T)
  code=paste0(code,"\n}")
  code
}
