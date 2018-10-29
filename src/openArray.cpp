#include "openArray.h"
#include "kernelManager.h"
#include <string> 
using namespace std;
openArray::openArray(size_t size1, dtype type):openArray(size1,1, type){}

openArray::openArray(size_t size1, size_t size2, dtype type)
{
	kernelManager::checkAndInitializeManager();
	device_id = kernelManager::getDeviceIndex();
	gpuAlloc(size1*size2, type);
	dimension[0] = size1;
	dimension[1] = size2;
	dataType = type;
}

openArray::openArray(size_t size1, void * src, dtype type) :openArray(size1, 1, src, type) {}


openArray::openArray(size_t size1, size_t size2, void * src, dtype type)
{
	kernelManager::checkAndInitializeManager();
	device_id = kernelManager::getDeviceIndex();
	void* host_data = malloc(size1*size2*typesize(type));
	RTogpu(host_data, src, type, size1*size2);
	gpuAlloc(size1*size2, host_data, type);
	dimension[0] = size1;
	dimension[1] = size2;
	dataType = type;
	free(host_data);
}




openArray::~openArray()
{
	releaseHostData();
	if (data == nullptr) return;
	clReleaseMemObject(data);
	delete[] dimension;
}


openArray* openArray::constant(double number, size_t size1, dtype type)
{
	openArray* oa=new openArray(size1, type);
	cl_command_queue queue = kernelManager::getQueue(oa->getDeviceId());
	void* tmp_data = convertDataType(number, type);
	cl_int error=clEnqueueFillBuffer(queue, *(*oa).getDeviceData(), tmp_data, typesize(type), 0, size1*typesize(type), 0, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle("An error has occured in memory assignment!");
	free(tmp_data);
	return oa;
}

openArray* openArray::constant(double number, size_t size1, size_t size2, dtype type)
{
	openArray* oa = constant(number, size1*size2, type);
	oa->resize(size1,size2);
	return oa;
}

cl_mem* openArray::getDeviceData()
{
	return &data;
}

void openArray::getHostData(void *source)
{
	if (data == nullptr) return;
	size_t size = dimension[0] * dimension[1] * typesize(dataType);
	cl_command_queue queue = kernelManager::getQueue(getDeviceId());
	cl_int error=clEnqueueReadBuffer(queue, data, CL_TRUE, 0, size, source, 0, NULL, NULL);
	if (error != CL_SUCCESS)errorHandle(string("Error in read GPU memory, error info:")+ getErrorString(error));
}

void * openArray::getHostData()
{
	size_t size = dimension[0] * dimension[1] * typesize(dataType);
	void* hostData = malloc(size);
	getHostData(hostData);
	hostptr.push_back(hostData);
	return hostData;
}

dtype openArray::getDataType()
{
	return dataType;
}

int openArray::getDeviceId()
{
	if (device_id == -1) {
		kernelManager::checkAndInitializeManager();
		device_id = kernelManager::getDeviceIndex();
	}
	return device_id;
}

size_t openArray::dims(int i)
{
	return dimension[i];
}

void openArray::resize(size_t size1, size_t size2)
{
	dimension[0] = size1;
	dimension[1] = size2;
}

void openArray::releaseHostData() {
	for (unsigned int i = 0; i < hostptr.size(); i++) {
		free(hostptr[i]);
	}
	hostptr.clear();
}


void openArray::gpuAlloc(size_t size, dtype type)
{
	cl_context context = kernelManager::getContext(getDeviceId());
	cl_int error;
	data=clCreateBuffer(context, CL_MEM_READ_WRITE, size*typesize(type), nullptr, &error);
	if (error != CL_SUCCESS) {
		string errorInfo =string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024)+
			"MB memory on device, error info: "+ getErrorString(error);
		errorHandle(errorInfo);
	}
}

void openArray::gpuAlloc(size_t size, void * hostData, dtype type)
{
	cl_context context = kernelManager::getContext(getDeviceId());
	cl_int error;
	data=clCreateBuffer(context, CL_MEM_READ_WRITE| CL_MEM_COPY_HOST_PTR, size*typesize(type), hostData, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024) + 
			"MB memory on device, error info: " + getErrorString(error);
		errorHandle(errorInfo);
	}
}



int openArray::typesize(dtype type)
{
	switch (type)
	{
	case c:
		return 1;
	case f16:
		return 2;
	case f32:
	case i32:
	case ui32:
		return 4;
	case f64:
	case i64:
	case ui64:
		return 8;
	default:
		errorHandle("Unsupported type");
	}
}

void * openArray::convertDataType(double data, dtype type)
{
	switch (type)
	{
	case c:
	{
		cl_char* tmp = (cl_char*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case f16:
	{
		cl_half* tmp = (cl_half*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case f32:
	{
		cl_float* tmp = (cl_float*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case i32:
	{
		cl_int* tmp = (cl_int*)malloc(typesize(type));
		*tmp = data;
		return tmp; 
	}
	case ui32:
	{
		cl_uint* tmp = (cl_uint*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case f64:
	{
		cl_double* tmp = (cl_double*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case i64:
	{
		cl_ulong* tmp = (cl_ulong*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	case ui64:
	{
		cl_int* tmp = (cl_int*)malloc(typesize(type));
		*tmp = data;
		return tmp;
	}
	default:
		errorHandle("The given type is not supported");
	}
}

