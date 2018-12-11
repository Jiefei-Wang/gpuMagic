#include "openArray.h"
#include "kernelManager.h"
#include <string> 
using namespace std;
openArray::openArray(size_t length, dtype type){
	kernelManager::checkAndInitializeManager();
	device_id = kernelManager::getDeviceIndex();
	gpuAlloc(length, type);
	this->length = length;
	dataType = type;
}


openArray::openArray( void * src, size_t length, dtype type)
{
	kernelManager::checkAndInitializeManager();
	device_id = kernelManager::getDeviceIndex();
	void* host_data = malloc(length*typesize(type));
	RTogpu(host_data, src, type, length);
	gpuAlloc(host_data, length, type);
	this->length = length;
	dataType = type;
	free(host_data);
}





openArray::~openArray()
{
	releaseHostData();
	if (data == nullptr) return;
	clReleaseMemObject(data);
}


openArray* openArray::constant(double number, size_t length, dtype type)
{
	openArray* oa=new openArray(length, type);
	cl_command_queue queue = kernelManager::getQueue(oa->getDeviceId());
	void* tmp_data = malloc(typesize(type));
	RTogpu(tmp_data, &number, type, 1);
	cl_int error=clEnqueueFillBuffer(queue, *(*oa).getDeviceData(), tmp_data, typesize(type), 0, length*typesize(type), 0, NULL, NULL);
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
	size_t size = length * typesize(dataType);
	cl_command_queue queue = kernelManager::getQueue(getDeviceId());
	cl_int error=clEnqueueReadBuffer(queue, data, CL_TRUE, 0, size, source, 0, NULL, NULL);
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

int openArray::getDeviceId()
{
	if (device_id == -1) {
		kernelManager::checkAndInitializeManager();
		device_id = kernelManager::getDeviceIndex();
	}
	return device_id;
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
	cl_context context = kernelManager::getContext(getDeviceId());
	cl_int error;
	data=clCreateBuffer(context, CL_MEM_READ_WRITE, size*typesize(type), NULL, &error);
	if (error != CL_SUCCESS) {
		string errorInfo =string("Fail to allocate ") +
			to_string(size*typesize(type) / 1024 / 1024)+
			"MB memory on device, error info: "+ getErrorString(error);
		errorHandle(errorInfo);
	}
}

void openArray::gpuAlloc(void * hostData,size_t length, dtype type)
{
	cl_context context = kernelManager::getContext(getDeviceId());
	cl_int error;
	data = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, length*typesize(type), hostData, &error);
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
}


