kernel void vectorAdd(gAuto1* a,gAuto2* b,gAuto3* c){
  int i=get_global_id(0);
 c[i]=a[i]+b[i];
  
}