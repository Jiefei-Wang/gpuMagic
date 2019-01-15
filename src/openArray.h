#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <vector>
#include "Tools.h"
#include "kernelManager.h"

class openArray {
private:
	deviceIdentifier device_id;
	cl_mem data = nullptr;
	size_t length;
	dtype dataType;
	std::vector<void*> hostptr;

public:
	openArray(deviceIdentifier device, size_t length, dtype type);
	openArray(deviceIdentifier device, void* src, size_t length, dtype type);
	static openArray* constant(deviceIdentifier device, double number, size_t length, dtype type);

	~openArray();
	//set and get data
	cl_mem* getDeviceData();
	//The source pointer must has the same size as the data size
	void getHostData(void *source);
	void* getHostData();
	dtype getDataType();
	//Dimension
	size_t getLength();
	//get the total size in byte
	size_t getTotalSize();
	//release the host data
	void releaseHostData();


private:
	void gpuAlloc(size_t size, dtype type);
	void gpuAlloc(void * hostData, size_t length, dtype type);
	//Convert the data type to a given type(Single value)

};


