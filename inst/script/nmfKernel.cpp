#define AUTO float
#define gATUO global float
#define lATUO local float

inline int ind(int i,int j,int nrow){
  return i+j*nrow;
  //return mad_sat(j,nrow,i);
}



kernel void MatMul1(gATUO* W,gATUO *V, gATUO* H,
global int* dimNRM,
lATUO* w_local, lATUO * tmp_m1,
lATUO * tmp_r, global int* size,gATUO* debug){
  int colId=get_local_id(0);
  int groupSize=get_local_size(0);
  int rowId=get_group_id(0);
  
  //if(colId==1)
  //  printf("%d,",length);
  
  int n=dimNRM[0];
  int r=dimNRM[1];
  int m=dimNRM[2];
  
  for(int i=colId;i<m;i=i+groupSize){
    tmp_m1[i]=0;
  }
  for(int i=colId;i<r;i=i+groupSize){
    tmp_r[i]=0;
  }
  
  int size_private=size[0];
  int loopNum=ceil((float)n/size_private);
  int startW=0;
  int endW=0;
  int length=0;
  AUTO tmp_element;
  for(int i=0;i<loopNum;i++){
    startW=endW;
    endW=endW+size_private;
    if(endW>n)
      endW=n;
    length=endW-startW;
    
    for(int j=colId;j<length;j=j+groupSize){
      w_local[j]=W[ind(j+startW,rowId,n)];
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    //Compute tmp_m1=t(W)%*%V
      for(int tmp_col=colId;tmp_col<m;tmp_col=tmp_col+groupSize){
        //printf("%d,",tmp_col);
        tmp_element=0;
        for(int j=0;j<length;j++){
          tmp_element=tmp_element+w_local[j]*V[ind(j+startW,tmp_col,n)];
        }
        tmp_m1[tmp_col]=tmp_m1[tmp_col]+tmp_element;
      }
    //compute tmp_r=t(W)%*%W
      for(int tmp_col=colId;tmp_col<r;tmp_col=tmp_col+groupSize){
        //printf("%d,",tmp_col);
        tmp_element=0;
        for(int j=0;j<length;j++){
          tmp_element=tmp_element+w_local[j]*W[ind(j+startW,tmp_col,n)];
        }
        tmp_r[tmp_col]=tmp_r[tmp_col]+tmp_element;
      }
      barrier(CLK_LOCAL_MEM_FENCE);
  }
  
  //compute tmp_m1*H
  for(int tmp_col=colId;tmp_col<m;tmp_col=tmp_col+groupSize){
    tmp_m1[tmp_col]=tmp_m1[tmp_col]*H[ind(rowId,tmp_col,r)];
  }
  //compute tmp_m1/(tmp_r%*%H)
  for(int tmp_col=colId;tmp_col<m;tmp_col=tmp_col+groupSize){
    //printf("%d,",tmp_col);
    tmp_element=0;
    for(int j=0;j<r;j++){
      tmp_element=tmp_element+tmp_r[j]*H[ind(j,tmp_col,r)];
    }
    tmp_m1[tmp_col]=tmp_m1[tmp_col]/tmp_element;
  }
  
  
 
  barrier(CLK_LOCAL_MEM_FENCE);
  
  for(int tmp_col=colId;tmp_col<m;tmp_col=tmp_col+groupSize){
    H[ind(rowId,tmp_col,r)]=tmp_m1[tmp_col];
  }
}


kernel void MatMul2(gATUO* W,gATUO *V, gATUO* H,
                    global int* dimNRM,
                    lATUO* h_local,lATUO * tmp_r,
                    gATUO* debug){
  int rowId=get_local_id(0);
  int groupSize=get_local_size(0);
  int colId=get_group_id(0);
  
  //if(colId==1)
  //printf("%d,",rowId);
  
  int n=dimNRM[0];
  int r=dimNRM[1];
  int m=dimNRM[2];
  
  //Initialize the vector
  for(int i=rowId;i<m;i=i+groupSize){
    h_local[i]=H[ind(colId,i,r)];
  }
  
  
  barrier(CLK_LOCAL_MEM_FENCE);
  
  AUTO tmp_element;
  int tmp_row;
  //compute tmp_r=H%*%t(H)
  for(tmp_row=rowId;tmp_row<r;tmp_row=tmp_row+groupSize){
    //printf("%d,",tmp_col);
    tmp_element=0;
    for(int j=0;j<m;j++){
      tmp_element=tmp_element+H[ind(tmp_row,j,r)]*h_local[j];
      //tmp_element=mad(H[ind(tmp_row,j,r)],h_local[j],tmp_element);
    }
    tmp_r[tmp_row]=tmp_element;
  }
  barrier(CLK_LOCAL_MEM_FENCE);
  
 
  AUTO tmp_m1=0;
  for (int i = 0; rowId + i*groupSize<n; i++) {
    tmp_row = rowId + i*groupSize;
    tmp_element = 0;
    //Compute tmp_m1=V%*%t(H)
    for (int j = 0; j<m; j++) {
      tmp_element = tmp_element + V[ind(tmp_row, j, n)] * h_local[j];
      //tmp_element=mad(V[ind(tmp_row, j, n)],h_local[j],tmp_element);
    }
    tmp_m1 = tmp_element;
    tmp_element = 0;
    //Compute tmp_m1=tmp_m1/(W%*%tmp_r)
    for (int j = 0; j<r; j++) {
      tmp_element = tmp_element + W[ind(tmp_row, j, n)] * tmp_r[j];
      //tmp_element=mad(W[ind(tmp_row, j, n)],tmp_r[j],tmp_element);
    }
    //Compute tmp_m1=W%*%tmp_m1
    tmp_m1 = tmp_m1 / tmp_element*W[ind(tmp_row, colId, n)];
    W[ind(tmp_row, colId, n)] = tmp_m1;
  }
  /*
  for(tmp_row=rowId;tmp_row<r;tmp_row=tmp_row+groupSize){
    debug[ind(tmp_row,colId,r)]=tmp_r[tmp_row];
  }*/
  
//barrier(CLK_LOCAL_MEM_FENCE);
  
}

