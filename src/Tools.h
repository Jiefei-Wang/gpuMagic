#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <iostream>
//#include <Rcpp.h>
template<class T1, class T2>
void cpyData(T1* target, T2* src, size_t n) {
	for (size_t i = 0; i < n; i++) {
		target[i] = src[i];
	}
}


template<class T>
void print_partial_matrix(const char* title, T *M, int nrows, int ncols, int max_row=-1,
	int max_col=-1)
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

void errorHandle(std::string errorInfo);
void message(std::string msg);



