kernel void test_scalar(gAuto1 *val,gAuto2 *num) {
  size_t i = get_global_id(0);
  auto2 lt=*num;
  auto1 tmp=val[i];
  for(uint j1=0;j1<1000;j1++){
    for (uint j = 0; j < lt; j++) {
      tmp=2*sqrt(tmp);
    }
  }
  val[i]=tmp;
}

kernel void test_vector(gAuto1 *val,gAuto2 *num) {
  size_t i = get_global_id(0);
  auto2 lt=*num;
  ulong k=4*i;
  auto1_v4 tmp=(auto1_v4)(val[k],val[k+1],val[k+2],val[k+3]);
  
  tmp=2*sqrt(tmp);
  
  val[k]=tmp.s0;
  val[k+1]=tmp.s1;
  val[k+2]=tmp.s2;
  val[k+3]=tmp.s3;
}

#define ONE noise[0]*noise[1]*noise[2]/noise[3]/noise[4]/noise[5]
/*
kernel void hoist(gAuto1 *data,
                         gAuto2* size,
                         global const auto3* noise
                         )
{
  int length=size[0];
  //int tmp=noise[0]*noise[1]*noise[2]/noise[3]/noise[4]/noise[5]-1;
  for(uint i=0;i<length;i++){
    int ind=i*ONE*ONE*ONE*ONE+ONE-1;
    data[ind]=data[ind]+data[ind];
  }
}
kernel void hoist1(gAuto1 *data,
                  gAuto2* size,
                  gAuto3* noise
)
{
  int length=size[0];
  int tmp1=ONE-1;
  int tmp2=ONE*ONE*ONE*ONE;
  for(uint i=0;i<length;i++){
    int ind=i*1+0;
    data[ind]=data[ind]+data[ind];
  }
}
*/
#define vectorSize 16
kernel void concurrent(gAuto1 *data,
                  gAuto2* size)
{
  int length=size[0];
  auto1 tmp[vectorSize];
  auto1_v4* tmp1=tmp;
  uint ind;
  for(uint i=0;i<length/vectorSize;i++){
    ind=i*vectorSize;
    for(uint j=0;j<vectorSize;j++){
      tmp[j]=data[ind+j];
    }
    for(uint k=0;k<vectorSize/4;k++){
      tmp1[k]=tmp1[k]*tmp1[k];
    }
    for(uint j=0;j<vectorSize;j++){
      data[ind+j]=tmp[j];
    }
  }
  
}


#define vectorSize 16
kernel void concurrent1(gAuto1 *data,
                       gAuto2* size)
{
  int length=size[0];
  auto1 tmp[vectorSize];
  auto1_v4* tmp1=tmp;
  uint ind;
  for(uint i=0;i<length/vectorSize;i++){
    ind=i*vectorSize;
    for(uint k=0;k<vectorSize/4;k++){
      tmp1[k]=(auto1_v4)(data[ind+4*k],data[ind+4*k+1],data[ind+4*k+2],data[ind+4*k+3]);
    }
    for(uint k=0;k<vectorSize/4;k++){
      tmp1[k]=tmp1[k]*tmp1[k];
    }
    for(uint j=0;j<vectorSize;j++){
      data[ind+j]=tmp[j];
    }
  }
  
}


kernel void test(){
#define test1 printf("this is test1")
  test1;
#define test1 printf("this is test2")
  test1;
}


