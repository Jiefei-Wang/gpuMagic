#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "R_ext/libextern.h"
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include"Tools.h"
#include "kernelManager.h"
#include "openArray.h"

//using namespace std;



extern "C" LibExport
void upload(void* data, double * dim, int* type,void** address) {
	openArray* matrix=nullptr;
	switch(*type) {
	case dtype::f32:
		matrix = new openArray(dim[0], dim[1],(double*)data, dtype::f32);
		break;
	case dtype::f64:
		matrix = new openArray(dim[0], dim[1], (double*)data, dtype::f64);
		break;
	case dtype::i32:
		matrix = new openArray(dim[0], dim[1], (int*)data, dtype::i32);
		break;
	case dtype::i64:
		matrix = new openArray(dim[0], dim[1], (double*)data, dtype::i64);
		break;
	};
	*address = (void*)matrix;
}
extern "C" LibExport
void download(void* data,int* type, void** address) {
	openArray* matrix = *(openArray**)address;
	//These two type can be directly send to R
	if (*type == dtype::f64|| *type == dtype::i32) {
		matrix->getHostData(data);
		return;
	}

	//These two type should be transfer to an R-compatible type
	void* host_data = matrix->getHostData();
	switch (matrix->getDataType()) {
	case dtype::f32:
		cpyData((double*)data, (float*)host_data, matrix->dims(0)*matrix->dims(1));
		break;
	case dtype::i64:
		cpyData((double*)data, (long long*)host_data, matrix->dims(0)*matrix->dims(1));
		break;
	};
	
}

extern "C" LibExport
void clear(void** address) {
	delete *(openArray**)address;
}
extern "C" LibExport
void hasKernel(char** signature, char** kernel, bool* res) {
	*res = kernelManager::hasKernel(std::string(*signature), std::string(*kernel));
}


extern "C" LibExport
void createKernel(char** signature, char** kernel, char** code) {
	//message(std::string(*code));
	kernelManager::createKernel(std::string(*signature), std::string(*kernel), std::string(*code));
}

extern "C" LibExport
void loadParameter(char** signature, char** kernel,void** address,int *ind) {
	cl_kernel dev_kernel= kernelManager::getKernel(std::string(*signature), std::string(*kernel));
	openArray* matrix = *(openArray**)address;
	cl_int error = clSetKernelArg(dev_kernel, *ind, sizeof(cl_mem), matrix->getDeviceData());
	if (error != CL_SUCCESS) errorHandle(std::string("kernel parameter uploading failure, error info:" + std::string(kernelManager::getErrorString(error))));
}

extern "C" LibExport
void launchKernel(char** signature, char** kernel, int* blockSize, int* threadSize) {
	cl_kernel dev_kernel = kernelManager::getKernel(std::string(*signature), std::string(*kernel));
	cl_command_queue queue = kernelManager::getQueue();
	size_t global_item_size = *blockSize; // Process the entire lists
	size_t local_item_size = *threadSize;
	cl_int error = clEnqueueNDRangeKernel(queue, dev_kernel, 1, NULL,
		&global_item_size, &local_item_size, 0, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle(std::string("kernel launch failure, error info:"+ std::string(kernelManager::getErrorString(error))));
}




extern "C" LibExport
void getDeviceList() {
	kernelManager::getAllDeviceName();
}
extern "C" LibExport
void getDeviceInfo(int * i) {
	kernelManager::getDeviceInfo( *i);
}
extern "C" LibExport
void getDeviceDetail(int * i) {
	kernelManager::getDeviceFullInfo(*i);
}
extern "C" LibExport
void setDevice(int * i) {
	kernelManager::setDevice(*i);
}

extern "C" LibExport
void getCurDevice() {
	kernelManager::getCurDevice();
}

extern "C" LibExport
void debug() {
	
}

int main(void) {
	char* src = "double add(double A, double B){ return A+B; } __kernel void vector_add__global (double *A, __global  double *B, __global  double *C)  { int i = get_global_id(0); C[i] =add(A+i,B+i); }";
	char* sig = "a";
	char* kernel = "vector_add";
	createKernel(&sig, &kernel, &src);
}


void test1() {

	{
		double data[] = { 1,2,3 };
		double dim[] = { 3,1 };
		int type[] = { dtype::f32 };
		void** ad = new void*;
		upload(data, dim, type, ad);
		double* data1 = new double[3];
		download(data1, type, ad);
		print_partial_matrix("f32", data1, 1, 3);
	}
	{
		double data[] = { 1,2,3 };
		double dim[] = { 3,1 };
		int type[] = { dtype::f64 };
		void** ad = new void*;
		upload(data, dim, type, ad);
		double* data1 = new double[3];
		download(data1, type, ad);
		print_partial_matrix("f64", data1, 1, 3);
	}
	{
		int data[] = { 1,2,3 };
		double dim[] = { 3,1 };
		int type[] = { dtype::i32 };
		void** ad = new void*;
		upload(data, dim, type, ad);
		int* data1 = new int[3];
		download(data1, type, ad);
		print_partial_matrix("i32", data1, 1, 3);
	}
	{
		double data[] = { 1,2,3 };
		double dim[] = { 3,1 };
		int type[] = { dtype::i64 };
		void** ad = new void*;
		upload(data, dim, type, ad);
		double* data1 = new double[3];
		download(data1, type, ad);
		print_partial_matrix("i64", data1, 1, 3);
	}
}