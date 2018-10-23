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
void createKernel(const char** signature, const char** kernel, const char** code) {
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
  const char* src = "__kernel void gpu_kernel0(__global double* gpu_worker_data,__global double* A,__global double* B,__global float* gpuMagic_tmp,__global long* gpu_tmp_length_arg,__global long* gpu_matrix_offSize,__global long* gpu_matrix_size1,__global long* gpu_matrix_size2,__global double* gpu_return_variable,__global long* gpu_return_size){\nunsigned long gpu_global_id=get_global_id(0);\nunsigned long gpu_tmp_length=*gpu_tmp_length_arg;\nunsigned long gpu_worker_offset=gpu_global_id*gpu_tmp_length;\ndouble opencl_tmp_1;\ndouble ind;\ndouble j;\nint opencl_tmp_2;\n__global double* C=(__global double*)(gpuMagic_tmp+gpu_worker_offset+gpu_matrix_offSize[0]);\nint opencl_tmp_3;\nint opencl_tmp_4;\ndouble opencl_tmp_5;\ndouble opencl_tmp_7;\ndouble opencl_tmp_8;\ndouble opencl_tmp_6;\ndouble opencl_tmp_9;\nopencl_tmp_1=gpu_global_id+1;\nind=gpu_worker_data[(unsigned int)opencl_tmp_1-1];\nj=ind;\nopencl_tmp_2=gpu_matrix_size1[1];\n\nfor(unsigned int gpu_loop_ind_0=1;gpu_loop_ind_0<=opencl_tmp_2;gpu_loop_ind_0++){\nfor(unsigned int gpu_loop_ind_1=1;gpu_loop_ind_1<=1;gpu_loop_ind_1++){\nC[(unsigned int)gpu_loop_ind_0-1 +((unsigned int)gpu_loop_ind_1-1)*gpu_matrix_size1[12]]=0;\n}\n}\nopencl_tmp_3=gpu_matrix_size1[1];\nfor(unsigned int gpu_loop_ind_2=1;gpu_loop_ind_2<=opencl_tmp_3;gpu_loop_ind_2++){\nopencl_tmp_4=gpu_matrix_size2[1];\nfor(unsigned int gpu_loop_ind_3=1;gpu_loop_ind_3<=opencl_tmp_4;gpu_loop_ind_3++){\nopencl_tmp_5=C[(unsigned int)gpu_loop_ind_2-1];\nopencl_tmp_7=A[(unsigned int)gpu_loop_ind_2-1 +((unsigned int)gpu_loop_ind_3-1)*gpu_matrix_size1[1]];\nopencl_tmp_8=B[(unsigned int)gpu_loop_ind_3-1 +((unsigned int)j-1)*gpu_matrix_size1[2]];\nopencl_tmp_6=opencl_tmp_7*opencl_tmp_8;\nopencl_tmp_9=opencl_tmp_5+opencl_tmp_6;\nC[(unsigned int)gpu_loop_ind_2-1]=opencl_tmp_9;\n}\n}\nfor(unsigned long gpu_return_i=0;gpu_return_i<*gpu_return_size;gpu_return_i++){\n\ngpu_return_variable[gpu_return_i+gpu_global_id*(*gpu_return_size)]=C[gpu_return_i];\n}\n}";
	const char* sig = "a";
	const char* kernel = "gpu_kernel0";
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
