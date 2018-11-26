#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <vector>
#include "Tools.h"


class openArray {
private:
	int device_id;
	cl_mem data = nullptr;
	size_t length = NULL;
	dtype dataType;
	std::vector<void*> hostptr;

public:
	openArray(size_t length, dtype type);
	openArray(void* src, size_t length, dtype type);
	~openArray();
	//set and get data
	cl_mem* getDeviceData();
	void getHostData(void *source);
	void* getHostData();
	dtype getDataType();
	int getDeviceId();
	//Dimension
	size_t getLength();
	//release the host data
	void releaseHostData();


	//Some special data creating method
	static openArray* constant(double number, size_t length, dtype type);
	static openArray* repeatData(void* src, size_t length,size_t repeatNum, dtype type);
private:
	void gpuAlloc(size_t size, dtype type);
	void gpuAlloc(size_t size, void* hostData, dtype type);
	static int typesize(dtype type);
	//Convert the data type to a given type(Single value)

};


