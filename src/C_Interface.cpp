

#include "kernelManager.h"
#include "openArray.h"
#include <string> 
#include <typeinfo>
using namespace std;
#include "Tools.h"

#include "C_Interface.h"

#define asString(x) string(CHAR(asChar(x)))

SEXP getPlatformNum()
{
	SEXP platformNum = ScalarInteger(kernelManager::getPlatformNum());
	return(platformNum);
}

SEXP getDeviceNum(SEXP platform)
{
	SEXP deviceNum = ScalarInteger(kernelManager::getDeviceNum(asInteger(platform)));
	return(deviceNum);
}

SEXP getDeviceInfo(SEXP platform, SEXP device)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	deviceInfo info = kernelManager::getDeviceInfo(id);
	SEXP info_R = PROTECT(allocVector(VECSXP, 7));

	SET_VECTOR_ELT(info_R, 0, PROTECT(mkString(info.device_name.c_str())));
	SET_VECTOR_ELT(info_R, 1, PROTECT(ScalarInteger(info.device_type)));
	SET_VECTOR_ELT(info_R, 2, PROTECT(ScalarReal(info.global_memory)));
	SET_VECTOR_ELT(info_R, 3, PROTECT(ScalarReal(info.local_memory)));
	SET_VECTOR_ELT(info_R, 4, PROTECT(ScalarLogical(info.has_local_memory)));
	SET_VECTOR_ELT(info_R, 5, PROTECT(mkString(info.opencl_version.c_str())));
	SET_VECTOR_ELT(info_R, 6, PROTECT(ScalarInteger(info.compute_unit_num)));

	unprotect(8);
	return(info_R);
}


SEXP upload(SEXP platform, SEXP device, SEXP data, SEXP length, SEXP type)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	dtype dataType = (dtype)asInteger(type);
	size_t dataLength = asReal(length);

	//printf("%d", TYPEOF(data));
	//
	bool needMalloc = true;
	void* gpuData;
	if (typeid(int) == typeid(cl_int) && TYPEOF(data) == INTSXP&&dataType ==dtype::i32) {
		needMalloc = false;
		gpuData = INTEGER(data);
	}
	if (typeid(double) == typeid(cl_double) && TYPEOF(data) == REALSXP&&dataType == dtype::f64) {
		needMalloc = false;
		gpuData = REAL(data);
	}
	if (needMalloc) {
		gpuData = malloc(dataLength*getTypeSize(dataType));
	}

	switch (TYPEOF(data)) {
	case LGLSXP:
		RTogpu(LOGICAL(data), gpuData, dataType, dataLength);
		break;
	case INTSXP:
		if(needMalloc)
			RTogpu(INTEGER(data), gpuData, dataType, dataLength);
		break;
	case REALSXP:
		if (needMalloc)
			RTogpu(REAL(data), gpuData, dataType, dataLength);
		break;
	case RAWSXP:
		RTogpu(RAW(data), gpuData, dataType, dataLength);
		break;
	default:
		error("Unsupported data structure!");
	}
	openArray* matrix = new openArray(id, gpuData, dataLength, dataType);
	SEXP result = R_MakeExternalPtr((void *)matrix, R_NilValue, R_NilValue);
	if(needMalloc)
		free(gpuData);
	return(result);
}


SEXP gpuMalloc(SEXP platform, SEXP device, SEXP length, SEXP type)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	openArray* matrix = new openArray(id, asReal(length), (dtype)asInteger(type));
	SEXP result = R_MakeExternalPtr((void *)matrix, R_NilValue, R_NilValue);
	return(result);
}

//c = 1, f16 = 2, f32 = 3, f64 = 4, i32 = 5, i64 = 6,
//ui32 = 7, ui64 = 8

SEXP download(SEXP address) {
	openArray* matrix = (openArray*)R_ExternalPtrAddr(address);
	dtype type = matrix->getDataType();
	size_t length = matrix->getLength();
	
	//The gpu date should be transfer to an R-compatible type
	//void* host_data = matrix->getHostData();
	SEXP res;

	switch (type) {
	case dtype::f16:
	case dtype::f32:
	case dtype::f64:
	case dtype::i64:
	case dtype::ui32:
	case dtype::ui64:
		res = PROTECT(allocVector(REALSXP, length));
		matrix->getHostData(REAL(res));
		if (type == dtype::f64&&typeid(double) == typeid(cl_double))
			break;
		//printf("type conversion");
		gpuToR(REAL(res), REAL(res), type, length, 1);
		break;
	case dtype::i32:
	case dtype::c:
		res = PROTECT(allocVector(INTSXP, length));
		matrix->getHostData(INTEGER(res));
		if (type == dtype::i32&&typeid(int) == typeid(cl_int))
			break;
		//printf("type conversion");
		gpuToR(INTEGER(res), INTEGER(res), type, length, 1);
		break;
	default:
		errorHandle("An unexpected data type has been found, please contact author for the help!");
	}

	UNPROTECT(1);
	return(res);
}




SEXP release(SEXP address) {
	openArray* matrix = (openArray*)R_ExternalPtrAddr(address);
	delete matrix;
	return(R_NilValue);
}


SEXP hasKernel(SEXP platform, SEXP device, SEXP signature, SEXP kernel) {
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	programSignature sig = { asString(signature) ,string(),string(),asString(kernel) };
	SEXP res = ScalarLogical(kernelManager::hasKernel(id, sig));
	return(res);
}




SEXP createKernel(SEXP platform, SEXP device, SEXP signature, SEXP flag, SEXP code, SEXP kernel) {
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	programSignature sig = { asString(signature) ,asString(flag),asString(code),asString(kernel) };
	//message(std::string(*code));
	//message(sig.compiler_flag);
	kernelManager::createKernel(id, sig);
	return(NILSXP);
}




SEXP setParameter(SEXP platform, SEXP device, SEXP signature, SEXP kernel, SEXP data_address, SEXP parm_index) {
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	programSignature sig = { asString(signature),string(),string(),asString(kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	openArray* matrix = (openArray*)R_ExternalPtrAddr(data_address);
	cl_int error = clSetKernelArg(dev_kernel, asInteger(parm_index), sizeof(cl_mem), matrix->getDeviceData());
	if (error != CL_SUCCESS)
		errorHandle(string("kernel parameter uploading failure, error info:") + string(getErrorString(error)));
	return(NILSXP);
}


SEXP setSharedParameter(SEXP platform, SEXP device, SEXP signature, SEXP kernel, SEXP size, SEXP parm_index) {
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	programSignature sig = { asString(signature),string(),string(),asString(kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	cl_int error = clSetKernelArg(dev_kernel, asInteger(parm_index), asInteger(size), NULL);
	//message(to_string(*size));
	if (error != CL_SUCCESS)
		errorHandle(string("kernel shared memory creating failure, error info:") + string(getErrorString(error)));
	return(NILSXP);
}



SEXP launchKernel(SEXP platform, SEXP device, SEXP signature, SEXP kernel, SEXP blockSize, SEXP threadSize) {
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	deviceContext& dc = kernelManager::getDevice(id);
	programSignature sig = { asString(signature),string(),string(),asString(kernel) };
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	size_t global_item_size = asInteger(blockSize); // Process the entire lists
	size_t local_item_size = asInteger(threadSize);
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
	return(NILSXP);

}


SEXP getPreferredGroupSize(SEXP platform, SEXP device, SEXP signature, SEXP kernel)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	programSignature sig = { asString(signature),string(),string(),asString(kernel) };
	cl_device_id cl_device = kernelManager::getDeviceId(id);
	cl_kernel dev_kernel = kernelManager::getKernel(id, sig);
	size_t prefferedSize;
	cl_int error = clGetKernelWorkGroupInfo(dev_kernel, cl_device, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
		sizeof(size_t), &prefferedSize, NULL);
	if (error != CL_SUCCESS)
		errorHandle(string("An error occured when query the preffered worker size, error info:") + string(getErrorString(error)));
	SEXP res = ScalarInteger(prefferedSize);
	return(res);
}


SEXP getDeviceStatus(SEXP platform, SEXP device)
{
	deviceIdentifier id = { asInteger(platform) ,asInteger(device) };
	deviceContext& dc = kernelManager::getDevice(id);
	int status;
	cl_int cl_status;
	if (dc.queue_event == NULL) {
		status = 0;
		return(ScalarInteger(status));
	}
	clGetEventInfo(dc.queue_event, CL_EVENT_COMMAND_EXECUTION_STATUS, sizeof(cl_int), &cl_status, NULL);

	switch (cl_status) {
	case CL_QUEUED:
		status = 3;
		break;
	case CL_SUBMITTED:
		status = 2;
		break;
	case CL_RUNNING:
		status = 1;
		break;
	case CL_COMPLETE:
		status = 0;
		break;
	default:
		status = cl_status;
	}
	return(ScalarInteger(status));
}


SEXP getTrueAd(SEXP ad)
{
	double* address=(double*)R_ExternalPtrAddr(ad);
	double ad_double = 0;
	memcpy(&ad_double, &address, sizeof(double*));
	return(ScalarReal(ad_double));
}

SEXP asMatrix(SEXP data, SEXP dim)
{
	setAttrib(data, R_DimSymbol, dim);
	return(NILSXP);
}






