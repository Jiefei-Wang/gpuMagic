#include "C_Interface.h"
#include "Tools.h"
#include "kernelManager.h"
#include "openArray.h"
#include <string> 
using namespace std;



void getCurDeviceIndex(int * id)
{
	*id = kernelManager::getDeviceIndex();
}
void upload(void* data, double * length, int* type, void** address) {
	
	openArray* matrix = new openArray(data,*length, (dtype)*type);
	*address = (void*)matrix;
}
void uploadWithRep(void * data, double * length, double * repNum, int * type, void ** address)
{
	*address=(void*)openArray::repeatData(data, *length, *repNum, (dtype)*type);
}

void gpuMalloc(double* length, int * type, void ** address)
{
	openArray* matrix = new openArray(*length, (dtype)*type);
	*address = (void*)matrix;
}


void download(void* data, void** address) {
	openArray* matrix = *(openArray**)address;
	dtype type = matrix->getDataType();
	//These two type can be directly send to R
	/*if (type == dtype::f64 || type == dtype::i32) {
		matrix->getHostData(data);
		return;
	}*/
	//These two type should be transfer to an R-compatible type
	void* host_data = matrix->getHostData();
	//print_partial_matrix("test:", (cl_int*)host_data, matrix->dims(0), matrix->dims(1));
	gpuToR(data, host_data, type, matrix->getLength());
	matrix->releaseHostData();
}



void clear(void** address) {
	delete *(openArray**)address;
}

void hasKernel(char** signature, char** kernel, bool* res) {
	*res = kernelManager::hasKernel(std::string(*signature), std::string(*kernel));
}



void createKernel(char** signature,char** kernel,  char** code,char** flag) {
	if (flag != nullptr)
		kernelManager::compiler_flag = string(*flag);
	else
		kernelManager::compiler_flag = string();
	//message(std::string(*code));
	kernelManager::createKernel(std::string(*signature), std::string(*kernel), std::string(*code));
}


void loadParameter(char** signature, char** kernel, void** data_address, int *parm_index) {
	cl_kernel dev_kernel = kernelManager::getKernel(std::string(*signature), std::string(*kernel));
	openArray* matrix = *(openArray**)data_address;
	cl_int error = clSetKernelArg(dev_kernel, *parm_index, sizeof(cl_mem), matrix->getDeviceData());
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));
}

void loadSharedParameter(char** signature, char** kernel, int* size, int *parm_index) {
	cl_kernel dev_kernel = kernelManager::getKernel(std::string(*signature), std::string(*kernel));
	cl_int error = clSetKernelArg(dev_kernel, *parm_index, *size, NULL);
	//message(to_string(*size));
	if (error != CL_SUCCESS)
		errorHandle(string("kernel shared memory creating failure, error info:") + string(getErrorString(error)));
}


void launchKernel(char** signature, char** kernel, int* blockSize, int* threadSize) {
	cl_kernel dev_kernel = kernelManager::getKernel(std::string(*signature), std::string(*kernel));
	cl_command_queue queue = kernelManager::getQueue(kernelManager::deviceIndex);
	size_t global_item_size = *blockSize; // Process the entire lists
	size_t local_item_size = *threadSize;
	cl_int error = clEnqueueNDRangeKernel(queue, dev_kernel, 1, NULL,
		&global_item_size, &local_item_size, 0, NULL, NULL);
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));
}





void getDeviceList() {
	kernelManager::getAllDeviceName();
}

void getDeviceInfo(int * i) {
	kernelManager::getDeviceInfo(*i);
}

void getDeviceDetail(int * i) {
	kernelManager::getDeviceFullInfo(*i);
}

void setDevice(int * i) {
	kernelManager::selectDevice(*i);
}


void getCurDevice() {
	kernelManager::getCurDevice();
}

void getDeviceSharedMem(int * id, double * mem)
{
	cl_ulong size;
	clGetDeviceInfo(kernelManager::getDevice(*id), CL_DEVICE_LOCAL_MEM_SIZE, sizeof(cl_ulong), &size, 0);
	*mem = size;
}


void debug(bool* test, int* length) {
	for (int i = 0; i < *length; i++) {
		cout << test[i] << endl;
	}
}