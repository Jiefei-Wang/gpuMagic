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


void errorHandle(std::string errorInfo);
void warningHandle(std::string warningInfo);
void message(std::string msg);
const char * getErrorString(cl_int error);
size_t getTypeSize(dtype type);





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

//site=0: copy from the start, site=1, copy from the end
template<class T1, class T2>
void cpyData(T1* target, T2* src, size_t n,int site=0) {
	if (site == 0) {
		for (size_t i = 0; i < n; i++) {
			target[i] = (T1)src[i];
		}
	}
	else {
		//printf("%d", n);
		size_t j;
		//printf("%d\n", src[0]);
		for (size_t i = 0; i < n; i++) {
			j = n - i-1;
			target[j] = (T1)src[j];

			//printf("j:%d, src:%d, target:%d\n", j,src[j], target[j]);
		}
	}
}



//Convert the data type between gpu and R data
template<class T>
void RTogpu(T* Rdata, void* gpuData, dtype type, size_t length,int site=0) {
	switch (type) {
	case dtype::c:
		cpyData((cl_uchar*)gpuData, Rdata, length, site);
		break;
	case dtype::f16:
		cpyData((cl_half*)gpuData, Rdata, length, site);
		break;
	case dtype::f32:
		cpyData((cl_float*)gpuData, Rdata, length, site);
		break;
	case dtype::f64:
		cpyData((cl_double*)gpuData, Rdata, length, site);
		break;
	case dtype::i32:
		cpyData((cl_int*)gpuData, Rdata, length, site);
		break;
	case dtype::i64:
		cpyData((cl_long*)gpuData, Rdata, length, site);
		break;
	case dtype::ui32:
		cpyData((cl_uint*)gpuData, Rdata, length, site);
		break;
	case dtype::ui64:
		cpyData((cl_ulong*)gpuData, Rdata, length, site);
		break;
	};
}
template<class T>
void gpuToR(void* gpuData, T* Rdata, dtype type, size_t length, int site = 0) {
	switch (type) {
	case dtype::c:
		cpyData(Rdata, (cl_uchar*)gpuData, length, site);
		break;
	case dtype::f16:
		cpyData(Rdata, (cl_half*)gpuData, length, site);
		break;
	case dtype::f32:
		cpyData(Rdata, (cl_float*)gpuData, length, site);
		break;
	case dtype::f64:
		cpyData(Rdata, (cl_double*)gpuData, length, site);
		break;
	case dtype::i32:
		cpyData(Rdata, (cl_int*)gpuData, length, site);
		break;
	case dtype::i64:
		cpyData(Rdata, (cl_long*)gpuData, length, site);
		break;
	case dtype::ui32:
		cpyData(Rdata, (cl_uint*)gpuData, length, site);
		break;
	case dtype::ui64:
		cpyData(Rdata, (cl_ulong*)gpuData, length, site);
		break;
	};
}


