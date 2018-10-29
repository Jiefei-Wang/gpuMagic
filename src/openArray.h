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
	size_t* dimension = new size_t[2];
	dtype dataType;
	std::vector<void*> hostptr;

public:
	openArray(size_t size1, dtype type);
	openArray(size_t size1, size_t size2, dtype type);
	openArray(size_t size1, void* src, dtype type);
	openArray(size_t size1, size_t size2, void* src, dtype type);
	~openArray();
	//set and get data
	static openArray* constant(double number, size_t size1, dtype type);
	static openArray* constant(double number, size_t size1, size_t size2, dtype type);
	cl_mem* getDeviceData();
	void getHostData(void *source);
	void* getHostData();
	dtype getDataType();
	int getDeviceId();
	//Dimension
	size_t dims(int i);
	void resize(size_t size1, size_t size2);
	//release the host data
	void releaseHostData();
private:
	void gpuAlloc(size_t size, dtype type);
	void gpuAlloc(size_t size, void* hostData, dtype type);
	static int typesize(dtype type);
	//Convert the data type to a given type(Single value)
	static void* convertDataType(double data, dtype type);

};


