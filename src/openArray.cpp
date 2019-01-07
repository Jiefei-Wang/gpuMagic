#include "openArray.h"
#include "kernelManager.h"
#include <string> 
using namespace std;
openArray::openArray(deviceIdentifier device, size_t length, dtype type){
	device_id = device;
	gpuAlloc(length, type);
	this->length = length;
	dataType = type;
}


openArray::openArray(deviceIdentifier device, void * src, size_t length, dtype type)
{
	device_id = device;
	void* host_data = malloc(length*typesize(type));
	RTogpu(host_data, src, type, length);
	gpuAlloc(host_data, length, type);
	this->length = length;
	dataType = type;
	free(host_data);
}

openArray* openArray::constant(deviceIdentifier device, double number, size_t length, dtype type)
{
	openArray* oa = new openArray(device,length, type);
	deviceContext dc = kernelManager::getDevice(device);
	void* tmp_data = malloc(typesize(type));
	void* host_ptr = malloc(8);
	//support float, double, long only
	switch (type) {
	case dtype::i32:
		*(int*)host_ptr = (int)number;
	case dtype::f64:
	case dtype::f32:
	case dtype::i64:
		*(double*)host_ptr = number;
		break;
	default:
		errorHandle("Unsupported data type!");
	}
	RTogpu(tmp_data, host_ptr, type, 1);
	cl_int error = clEnqueueFillBuffer(dc.command_queue, *(*oa).getDeviceData(), tmp_data, typesize(type), 0, length*typesize(type), 0, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle("An error has occured in memory assignment!");
	free(tmp_data);
	free(host_ptr);
	return oa;
}


openArray::~openArray()
{
	releaseHostData();
	if (data == nullptr) return;
	clReleaseMemObject(data);
}






cl_mem* openArray::getDeviceData()
{
	return &data;
}

void openArray::getHostData(void *source)
{
	if (data == nullptr) return;
	size_t size = length * typesize(dataType);
	deviceContext dc = kernelManager::getDevice(device_id);
	cl_int error=clEnqueueReadBuffer(dc.command_queue, data, CL_TRUE, 0, size, source, 0, NULL, NULL);
	if (error != CL_SUCCESS)errorHandle(string("Error in read GPU memory, error info:")+ getErrorString(error));
}

void * openArray::getHostData()
{
	size_t size = length * typesize(dataType);
	void* hostData = malloc(size);
	getHostData(hostData);
	hostptr.push_back(hostData);
	return hostData;
}

dtype openArray::getDataType()
{
	return dataType;
}


size_t openArray::getLength()
{
	return length;
}


void openArray::releaseHostData() {
	for (unsigned int i = 0; i < hostptr.size(); i++) {
		free(hostptr[i]);
	}
	hostptr.clear();
}


void openArray::gpuAlloc(size_t size, dtype type)
{
	deviceContext dc = kernelManager::getDevice(device_id);
	cl_int error;
	data=clCreateBuffer(dc.context, CL_MEM_READ_WRITE, size*typesize(type), NULL, &error);
	if (error != CL_SUCCESS) {
		string errorInfo =string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024)+
			"MB memory on device, error info: "+ getErrorString(error);
		errorHandle(errorInfo);
	}
}

void openArray::gpuAlloc(void * hostData,size_t length, dtype type)
{
	deviceContext dc = kernelManager::getDevice(device_id);
	cl_int error;
	data = clCreateBuffer(dc.context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, length*typesize(type), hostData, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to allocate ") +
			to_string(length*typesize(type) / 1024 / 1024) +
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
	return 0;
}


