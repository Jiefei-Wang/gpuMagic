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
	throw("test error");
}

int main(void) {
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
