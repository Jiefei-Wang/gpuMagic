#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include "CommonHeader.h"
#include <vector>
#include "Tools.h"
#include <type_traits>
enum dtype {autoDetect=1,f32=2,f64=3,i32=4,i64=5};


class openArray {
private:
	cl_mem data=nullptr;
	LARGEINDEX* dimension=new LARGEINDEX[2];
	dtype dataType;
	std::vector<void*> hostptr;
public:
	openArray(LARGEINDEX size1, dtype type);
	openArray(LARGEINDEX size1, LARGEINDEX size2, dtype type);
	template<class T>
	openArray(LARGEINDEX size1,T* src, dtype type);
	template<class T>
	openArray(LARGEINDEX size1, LARGEINDEX size2, T* src, dtype type);
	~openArray();
	//set and get data
	static openArray* constant(double number, LARGEINDEX size1, dtype type);
	static openArray* constant(double number, LARGEINDEX size1, LARGEINDEX size2, dtype type);
	cl_mem* getDeviceData();
	void getHostData(void *source);
	void* getHostData();
	dtype getDataType();
	//Dimension
	LARGEINDEX dims(int i);

private:
	template<class T>
	void* dataConvert(LARGEINDEX size, T data, dtype type);
	void gpuAlloc(LARGEINDEX size, dtype type);
	void gpuAlloc(LARGEINDEX size,void* hostData, dtype type);
	static int typesize(dtype type);
	static void* transferData(double data, dtype type);

};



template<class T>
inline openArray::openArray(LARGEINDEX size1, T * src, dtype type)
{
	if (type == dtype::autoDetect) {
		if (std::is_same<T, int>::value)	type = dtype::i32;
		if (std::is_same<T, long long>::value)type = dtype::i64;
		if (std::is_same<T, float>::value)type = dtype::f32;
		if (std::is_same<T, double>::value)type = dtype::f64;
		gpuAlloc(size1, src, type);
	}
	else {
		void* host_data = dataConvert(size1, src, type);
		gpuAlloc(size1, host_data, type);
	}
	dimension[0] = size1;
	dimension[1] = 1;
	dataType = type;
}

template<class T>
inline openArray::openArray(LARGEINDEX size1, LARGEINDEX size2, T * src, dtype type)
{
	if (type == dtype::autoDetect) {
		if (std::is_same<T, int>::value)	type = dtype::i32;
		if (std::is_same<T, long long>::value)type = dtype::i64;
		if (std::is_same<T, float>::value)type = dtype::f32;
		if (std::is_same<T, double>::value)type = dtype::f64;
		gpuAlloc(size1*size2, src, type);
	}
	else {
		void* host_data = dataConvert(size1*size2, src, type);
		gpuAlloc(size1*size2, host_data, type);
		free(host_data);
	}
	dimension[0] = size1;
	dimension[1] = size2;
	dataType = type;
}

template<class T>
void * openArray::dataConvert(LARGEINDEX size, T data, dtype type)
{
	
	switch (type) {
	case f32: {
		float* tmp_data = (float*)malloc(size*typesize(type));
		cpyData(tmp_data, data, size);
		return tmp_data;
	}
	case f64: {
		double* tmp_data = (double*)malloc(size*typesize(type));
		cpyData(tmp_data, data, size);
		return tmp_data;
	}
	case i32: {
		int* tmp_data = (int*)malloc(size*typesize(type));
		cpyData(tmp_data, data, size);
		return tmp_data;
	}
	case i64: {
		long long* tmp_data = (long long*)malloc(size*typesize(type));
		cpyData(tmp_data, data, size);
		return tmp_data;
	}
	};
}