#include "openArray.h"
#include "kernelManager.h"
#include <string> 
using namespace std;
openArray::openArray(LARGEINDEX size1, dtype type)
{
	gpuAlloc(size1, type);
	dimension[0] = size1;
	dimension[1] = 1;
	dataType = type;
}

openArray::openArray(LARGEINDEX size1, LARGEINDEX size2, dtype type)
{
	
	gpuAlloc(size1*size2, type);
	dimension[0] = size1;
	dimension[1] = size2;
	dataType = type;
}




openArray::~openArray()
{
	//release the host data
	for (unsigned int i = 0; i < hostptr.size(); i++) {
		free(hostptr[i]);
	}
	hostptr.clear();
	if (data == nullptr) return;
	clReleaseMemObject(data);
	delete[] dimension;
}

openArray* openArray::constant(double number, LARGEINDEX size1, dtype type)
{
	openArray* oa=new openArray(size1, type);
	cl_command_queue queue = kernelManager::getQueue();
	void* tmp_data = transferData(number, type);
	cl_int error=clEnqueueFillBuffer(queue, *(*oa).getDeviceData(), tmp_data, typesize(type), 0, size1*typesize(type), NULL, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle("An error has occured in memory assignment!");
	free(tmp_data);
	return oa;
}

openArray* openArray::constant(double number, LARGEINDEX size1, LARGEINDEX size2, dtype type)
{
	openArray* oa = new openArray(size1, size2, type);
	cl_command_queue queue = kernelManager::getQueue();
	void* tmp_data = transferData(number, type);
	cl_int error = clEnqueueFillBuffer(queue, *(*oa).getDeviceData(), tmp_data, typesize(type), 0, size1*size2*typesize(type), NULL, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle("An error has occured in memory assignment!");
	free(tmp_data);
	return oa;
}

cl_mem* openArray::getDeviceData()
{
	return &data;
}

void openArray::getHostData(void *source)
{
	if (data == nullptr) return;
	LARGEINDEX size = dimension[0] * dimension[1] * typesize(dataType);
	cl_command_queue queue = kernelManager::getQueue();
	cl_int error=clEnqueueReadBuffer(queue, data, CL_TRUE, 0, size, source, NULL, NULL, NULL);
	if (error != CL_SUCCESS)errorHandle("Error in read GPU memory");
}

void * openArray::getHostData()
{
	LARGEINDEX size = dimension[0] * dimension[1] * typesize(dataType);
	void* hostData = malloc(size);
	getHostData(hostData);
	hostptr.push_back(hostData);
	return hostData;
}

dtype openArray::getDataType()
{
	return dataType;
}

LARGEINDEX openArray::dims(int i)
{
	return dimension[i];
}



void openArray::gpuAlloc(LARGEINDEX size, dtype type)
{
	cl_context context = kernelManager::getContext();
	cl_int error;
	data=clCreateBuffer(context, CL_MEM_READ_WRITE, size*typesize(type), nullptr, &error);
	if (error != CL_SUCCESS) {
		string errorInfo =string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024)+
			"MB memory on device, error info: "+ kernelManager::getErrorString(error);
		errorHandle(errorInfo);
	}
}

void openArray::gpuAlloc(LARGEINDEX size, void * hostData, dtype type)
{
	cl_context context = kernelManager::getContext();
	cl_int error;
	data=clCreateBuffer(context, CL_MEM_READ_WRITE| CL_MEM_COPY_HOST_PTR, size*typesize(type), hostData, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024) + 
			"MB memory on device, error info: " + kernelManager::getErrorString(error);
		errorHandle(errorInfo);
	}
}

int openArray::typesize(dtype type)
{
	switch (type)
	{
	case f32:
		return 4;
	case f64:
		return 8;
	case i32:
		return 4;
	case i64:
		return 8;
	default:
		return 0;
	}
}

void * openArray::transferData(double data, dtype type)
{
	switch (type)
	{
	case f32: 
	{
		float* tmp = (float*)malloc(typesize(type));
		*tmp = data;
		return tmp; 
	}
	case f64:
	{
		double* tmp = (double*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case i32:
	{
		int* tmp = (int*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case i64:
	{
		long long* tmp = (long long*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	default:
		errorHandle("The given type is not supported");
	}
}

