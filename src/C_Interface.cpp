
#define _CRT_SECURE_NO_WARNINGS
#include "C_Interface.h"
#include "Tools.h"
#include "kernelManager.h"
#include "openArray.h"
#include <string> 
using namespace std;



void getPlatformNum(int* platformNum)
{
	*platformNum = kernelManager::getPlatformNum();
}
void getDeviceNum(int * platform, int* deviceNum)
{
	*deviceNum = kernelManager::getDeviceNum(*platform);
}
void getDeviceInfo(int * platform, int * device, char ** deviceName, int * deviceType, double * global_memory, double * local_memory, int * haslocalMemory, char ** opencl_version, int * compute_unit_num)
{
	deviceIdentifier id = { *platform ,*device };
	deviceInfo info = kernelManager::getDeviceInfo(id);
	strcpy(*deviceName, info.device_name.c_str());
	*deviceType = info.device_type;
	*global_memory = (double)info.global_memory;
	*local_memory = (double)info.local_memory;
	*haslocalMemory = info.has_local_memory;
	strcpy(*opencl_version, info.opencl_version.c_str());
	*compute_unit_num = info.compute_unit_num;
}



SEXP upload(SEXP platform, SEXP deviceNum, SEXP data, SEXP length, SEXP type)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(deviceNum) };
	void* dataAd;
	dtype dataType =(dtype) asInteger(type);
	message(to_string(asReal(length)));
	switch (dataType) {
	case dtype::c:
		dataAd=RAW(data);
		break;
	case dtype::f16:
	case dtype::f32:
	case dtype::f64:
	case dtype::i64:
	case dtype::ui32:
	case dtype::ui64:
		dataAd=REAL(data);
		break;
	case dtype::i32:
		dataAd=INTEGER(data);
		break;
	}
	//openArray* matrix = new openArray(id, dataAd, (size_t)asReal(length), dataType);
	//SEXP result = PROTECT(R_MakeExternalPtr((void *)matrix, NULL, NULL));
	//UNPROTECT(1);
	
	//return(ScalarInteger(1));
	
	//*address = (void*)matrix;
}

void gpuMalloc(int* platform, int* deviceNum, double* length, int * type, void ** address)
{
	deviceIdentifier id = { *platform ,*deviceNum };
	openArray* matrix = new openArray(id, (size_t)*length, (dtype)*type);
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
	//The gpu date should be transfer to an R-compatible type
	void* host_data = matrix->getHostData();
	//print_partial_matrix("test:", (cl_int*)host_data, matrix->dims(0), matrix->dims(1));
	gpuToR(data, host_data, type, matrix->getLength());
	matrix->releaseHostData();
}



void release(void** address) {
	delete *(openArray**)address;
}

void hasKernel(int* platform, int* deviceNum, char** signature, char** kernel, bool* res) {
	deviceIdentifier id = { *platform ,*deviceNum };
	programSignature sig = { string(*signature) ,string(),string(),string(*kernel) };
	*res = kernelManager::hasKernel(id, sig);
}



void createKernel(int* platform, int* deviceNum, char** signature, char** flag, char** code, char** kernel) {
	deviceIdentifier id = { *platform ,*deviceNum };
	programSignature sig = { string(*signature) ,string(*flag),string(*code),string(*kernel) };
	//message(std::string(*code));
	kernelManager::createKernel(id, sig);
}



void setParameter(int* platform, int* deviceNum, char** signature, char** kernel, void** data_address, int *parm_index) {
	deviceIdentifier id = { *platform ,*deviceNum };
	programSignature sig = { string(*signature),string(),string(),string(*kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	openArray* matrix = *(openArray**)data_address;
	cl_int error = clSetKernelArg(dev_kernel, *parm_index, sizeof(cl_mem), matrix->getDeviceData());
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));
}

void setSharedParameter(int* platform, int* deviceNum, char** signature, char** kernel, int* size, int *parm_index) {
	deviceIdentifier id = { *platform ,*deviceNum };
	programSignature sig = { string(*signature) ,string(),string(),string(*kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	cl_int error = clSetKernelArg(dev_kernel, *parm_index, *size, NULL);
	//message(to_string(*size));
	if (error != CL_SUCCESS)
		errorHandle(string("kernel shared memory creating failure, error info:") + string(getErrorString(error)));
}


void launchKernel(int* platform, int* deviceNum, char** signature, char** kernel, int* blockSize, int* threadSize) {
	deviceIdentifier id = { *platform ,*deviceNum };
	deviceContext& dc = kernelManager::getDevice(id);
	programSignature sig = { string(*signature) ,string(),string(),string(*kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	size_t global_item_size = *blockSize; // Process the entire lists
	size_t local_item_size = *threadSize;
	cl_int error;
	if (local_item_size == 0) {
		error = clEnqueueNDRangeKernel(dc.command_queue, dev_kernel, 1, NULL,
			&global_item_size, NULL, 0, NULL, &dc.queue_event);
	}
	else {
		error = clEnqueueNDRangeKernel(dc.command_queue, dev_kernel, 1, NULL,
			&global_item_size, &local_item_size, 0, NULL, &dc.queue_event);
	}
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));
	error=clFlush(dc.command_queue);
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));

}

void getPreferredGroupSize(int * platform, int * deviceNum, char ** signature, char ** kernel,double* res)
{
	deviceIdentifier id = { *platform ,*deviceNum };
	programSignature sig = { string(*signature) ,string(),string(),string(*kernel) };
	cl_device_id device = kernelManager::getDeviceId(id);
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	size_t prefferedSize;
	cl_int error = clGetKernelWorkGroupInfo(dev_kernel, device, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
		sizeof(size_t), &prefferedSize, NULL);
	if (error != CL_SUCCESS)
		errorHandle(string("An error occured when query the preffered worker size, error info:") + string(getErrorString(error)));
	*res = prefferedSize;
}

void getDeviceStatus(int * platform, int * deviceNum, int * status)
{
	deviceIdentifier id = { *platform ,*deviceNum };
	deviceContext& dc = kernelManager::getDevice(id);
	cl_int cl_status;
	if (dc.queue_event == NULL) {
		*status = 0;
		return;
	}
	clGetEventInfo(dc.queue_event, CL_EVENT_COMMAND_EXECUTION_STATUS, sizeof(cl_int), &cl_status, NULL);

	switch (cl_status) {
	case CL_QUEUED:
		*status = 3;
		break;
	case CL_SUBMITTED:
		*status = 2;
		break;
	case CL_RUNNING:
		*status = 1;
		break;
	case CL_COMPLETE:
		*status = 0;
		break;
	default:
		*status = cl_status;
	}
}






void getDeviceFullInfo(int * platform, int * deviceNum) {
	deviceIdentifier id = { *platform ,*deviceNum };
	kernelManager::getDeviceFullInfo(id);
}


