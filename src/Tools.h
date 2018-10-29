#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <iostream>


enum dtype {
	c = 1, f16 = 2, f32 = 3, f64 = 4, i32 = 5, i64 = 6,
	ui32 = 7, ui64 = 8
};

//Convert the data type between gpu and R data
void gpuToR(void* Rdata, void* gpuData, dtype type,size_t length);
void RTogpu(void* gpuData, void* Rdata, dtype type, size_t length);

void errorHandle(std::string errorInfo);
void message(std::string msg);
const char * getErrorString(cl_int error);




//#include <Rcpp.h>
template<class T1, class T2>
void cpyData(T1* target, T2* src, size_t n) {
	for (size_t i = 0; i < n; i++) {
		target[i] = src[i];
	}
}


template<class T>
void print_partial_matrix(std::string title, T *M, int nrows, int ncols, int max_row=-1,int max_col=-1)
{
	std::cout << title << std::endl;
	int row, col;
	if (max_row == -1) max_row = nrows;
	if (max_col == -1) max_col = ncols;
	for (row = 0; row < max_row; row++)
	{
		for (col = 0; col < max_col; col++)
		{
			std::cout << M[row + col*nrows] << " ";
		}
		printf("...\n");
	}
	printf("...\n");
}


