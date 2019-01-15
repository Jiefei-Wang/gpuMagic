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
	gpuAlloc(src, length, type);
	this->length = length;
	dataType = type;
}

openArray* openArray::constant(deviceIdentifier device, double number, size_t length, dtype type)
{
	openArray* oa = new openArray(device,length, type);
	deviceContext dc = kernelManager::getDevice(device);
	void* tmp_data = malloc(getTypeSize(type));
	RTogpu(&number,tmp_data, type, 1);
	cl_int error = clEnqueueFillBuffer(dc.command_queue, *(*oa).getDeviceData(), tmp_data, getTypeSize(type), 0, length*getTypeSize(type), 0, NULL, NULL);
	if (error != CL_SUCCESS) errorHandle("An error has occured in memory assignment!");
	free(tmp_data);
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
	size_t size = getTotalSize();
	deviceContext dc = kernelManager::getDevice(device_id);
	cl_int error=clEnqueueReadBuffer(dc.command_queue, data, CL_TRUE, 0, size, source, 0, NULL, NULL);
	if (error != CL_SUCCESS)errorHandle(string("Error in read GPU memory, error info:")+ getErrorString(error));
}

void * openArray::getHostData()
{
	size_t size = getTotalSize();
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

size_t openArray::getTotalSize()
{
	return getLength()*getTypeSize(getDataType());
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
	data=clCreateBuffer(dc.context, CL_MEM_READ_WRITE, size*getTypeSize(type), NULL, &error);
	if (error != CL_SUCCESS) {
		string errorInfo =string("Fail to allocate ") +
			to_string(size*getTypeSize(type) / 1024 / 1024)+
			"MB memory on device, error info: "+ getErrorString(error);
		errorHandle(errorInfo);
	}
}

void openArray::gpuAlloc(void * hostData,size_t length, dtype type)
{
	deviceContext dc = kernelManager::getDevice(device_id);
	cl_int error;
	data = clCreateBuffer(dc.context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, length*getTypeSize(type), hostData, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to allocate ") +
			to_string(length*getTypeSize(type) / 1024 / 1024) +
			"MB memory on device, error info: " + getErrorString(error);
		errorHandle(errorInfo);
	}
}


