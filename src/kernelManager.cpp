#include "kernelManager.h"
#include "Tools.h"
#include <string> 
int kernelManager::deviceIndex = -1;
cl_context kernelManager::context = nullptr;
cl_device_id kernelManager::device_id = nullptr;
cl_command_queue kernelManager::command_queue = nullptr;

map< int, cl_context> kernelManager::contextTable;
map< int, cl_device_id> kernelManager::deviceTable;
map< int, cl_command_queue> kernelManager::queueTable;
map<string, cl_program> kernelManager::programTable;
map<string, cl_kernel> kernelManager::kernelTable;




void kernelManager::getAllDeviceName() {
	cl_uint platform_num;
	clGetPlatformIDs(0, NULL, &platform_num);
	cl_platform_id* platform_id = new cl_platform_id[platform_num];
	clGetPlatformIDs(platform_num, platform_id, NULL);
	cl_uint device_num;
	cl_device_id* device_id;
	int device_count = 0;
	for (int i = 0; i < platform_num; i++) {
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, 0, NULL, &device_num);
		device_id = new cl_device_id[device_num];
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, device_num, device_id, NULL);
		size_t name_size;
		char* device_name;
		for (int j = 0; j < device_num; j++) {
			clGetDeviceInfo(device_id[j], CL_DEVICE_NAME, 0, NULL, &name_size);
			device_name = new char[name_size];
			clGetDeviceInfo(device_id[j], CL_DEVICE_NAME, name_size, device_name, NULL);
			string info = string("Device ") + to_string(device_count) + ": " + device_name;
			message(info);
			device_count++;
			delete[] device_name;
		}
		delete[] device_id;
	}
	delete[] platform_id;
	if (platform_num == 0) message("No device is available, do you forget to install the driver?");
}

void kernelManager::getDeviceInfo(int device_index)
{

	int strlen = 1024;
	char* buffer = new char[strlen];
	cl_device_id device = getDeviceID(device_index);
	if (device == nullptr) errorHandle("The given device is not found, please check if you have a opencl-enable device available!");
	(clGetDeviceInfo(device, CL_DEVICE_VENDOR, strlen, buffer, NULL));
	printf("Platform name: %s\n", buffer);
	(clGetDeviceInfo(device, CL_DEVICE_NAME, strlen, buffer, NULL));
	printf("Device name: %s\n", buffer);
	(clGetDeviceInfo(device, CL_DEVICE_OPENCL_C_VERSION, strlen, buffer, NULL));
	printf("Opencl version: %s\n", buffer);
	cl_ulong global_mem_size;
	(clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(global_mem_size), &global_mem_size, NULL));
	printf("Device memory size: %llu MB\n", global_mem_size / 1048576);
	delete[] buffer;
}

void kernelManager::getCurDevice()
{
	getDeviceInfo(deviceIndex);
}

void kernelManager::selectDevice(int device)
{
	//Try to find the device
	if (contextTable.find(device) == contextTable.end()) {
		initializeDevice(device);
	}
	deviceIndex = device;
	device_id = deviceTable[device];
	context = contextTable[device];
	command_queue = queueTable[device];
}

void kernelManager::destroyDevice(int device)
{
	if (contextTable.find(device) == contextTable.end()) return;
	cl_int error;
	error = clFlush(command_queue);
	error += clFinish(command_queue);
	for (map< string, cl_kernel>::iterator it = kernelTable.begin(); it != kernelTable.end(); ++it) {
		error += clReleaseKernel(it->second);
	}
	for (map< string, cl_program>::iterator it = programTable.begin(); it != programTable.end(); ++it) {
		error += clReleaseProgram(it->second);
	}
	programTable.clear();
	kernelTable.clear();
	error += clReleaseCommandQueue(queueTable[device]);
	error += clReleaseContext(contextTable[device]);
	queueTable.erase(device);
	contextTable.erase(device);
	deviceTable.erase(device);
	if (deviceIndex == device) {
		command_queue = nullptr;
		context = nullptr;
		device_id = nullptr;
		deviceIndex = -1;
	}
	if (error != CL_SUCCESS) errorHandle("An error has occured in releasing context");
}

int kernelManager::getDeviceIndex()
{
	return deviceIndex;
}

bool kernelManager::hasKernel(string signature, string kernel)
{
	if (deviceIndex == -1)
		return false;
	if (kernelTable.find(getSignature(signature, kernel)) != kernelTable.end())
		return true;
	else
		return false;
}



cl_kernel kernelManager::getKernel(string signature, string kernel)
{
	if (kernelTable.find(getSignature(signature, kernel)) != kernelTable.end())
		return kernelTable[getSignature(signature, kernel)];
	errorHandle("The given kernel does not find");
}

cl_kernel kernelManager::createKernel(string signature, string kernel, string code)
{
	string sig = getSignature(signature, kernel);
	if (kernelTable.find(sig) != kernelTable.end())
		return kernelTable[sig];
	cl_program program = loadProgram(sig, code);
	cl_int error;
	cl_kernel dev_kernel = clCreateKernel(program, kernel.c_str(), &error);

	switch (error) {
	case 0:
		break;
	default:
		string errorInfo = string("Fail to create kernel, error info: ") + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
	}

	kernelTable.insert(make_pair(sig, dev_kernel));
	return dev_kernel;
}


cl_program kernelManager::loadProgram(string signature, string code)
{

	checkAndInitializeManager();
	if (programTable.find(signature) != programTable.end())
		return programTable[signature];

	// create and build program
	cl_int error = 0;
	const char* source = code.c_str();
	//printf(source);
	cl_program program = clCreateProgramWithSource(context, 1, &source, 0, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to read program, error info: ") + to_string(error) + "-" + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
		return NULL;
	}
	error = clBuildProgram(program, 1, &device_id, 0, 0, 0);
	switch (error) {
	case CL_SUCCESS:
		break;
	case CL_BUILD_PROGRAM_FAILURE: {
		size_t strlen = 0;
		// Get the log
		clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &strlen);
		char *buffer = (char*)malloc(strlen*sizeof(char));
		clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, strlen, buffer, NULL);
		// Print the log
		string errorInfo = string("Fail to build program: ") + to_string(error) + "\n" + string(buffer);
		errorHandle(errorInfo);
		free(buffer);
		
	}
	default: {
		string errorInfo = string("Fail to build program: ") + to_string(error) + "-" + string(getErrorString(error));
		errorHandle(errorInfo);
	}
	}
	programTable.insert(make_pair(signature, program));
	return program;
}

cl_context kernelManager::getContext(int device)
{

	if (contextTable.find(device) == contextTable.end())
		initializeDevice();
	return contextTable[device];
}

cl_device_id kernelManager::getDevice(int device)
{
	if (deviceTable.find(device) == deviceTable.end())
		initializeDevice(device);
	return deviceTable[device];
}

cl_command_queue kernelManager::getQueue(int device)
{
	if (queueTable.find(device) == queueTable.end())
		initializeDevice(device);
	return queueTable[device];
}

void kernelManager::checkAndInitializeManager()
{
	if (deviceIndex == -1) {
		selectDevice(0);
	}
}



void kernelManager::initializeDevice(int device)
{
	if (deviceTable.find(device) != deviceTable.end())
		return;
	//If the device is not found, create the device
	cl_int error;
	//destroyDevice();
	cl_device_id curDevice_id = getDeviceID(device);
	if (curDevice_id == nullptr) errorHandle("The given device is not found, please check if you have an opencl-enable device available!");
	cl_context curContext = clCreateContext(NULL, 1, &curDevice_id, NULL, NULL, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a context associated with the current device!");
	cl_command_queue curCommand_queue = clCreateCommandQueue(curContext, curDevice_id, 0, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a command queue associated with the current device!");
	//Insert the device object to the table
	deviceTable.insert(make_pair(device, curDevice_id));
	contextTable.insert(make_pair(device, curContext));
	queueTable.insert(make_pair(device, curCommand_queue));
}


string kernelManager::getSignature(string sig, string kernel)
{
	return string("device") + to_string(deviceIndex) + sig + kernel;
}


//======================Some lengthy functions==============================
cl_device_id kernelManager::getDeviceID(int k)
{
	cl_int error = 0;
	cl_uint platform_num;
	error = clGetPlatformIDs(0, NULL, &platform_num);
	cl_platform_id* platform_id = new cl_platform_id[platform_num];
	error = clGetPlatformIDs(platform_num, platform_id, NULL);
	cl_uint device_num;
	cl_device_id* device_id;
	int device_count = 0;
	for (int i = 0; i < platform_num; i++) {
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, 0, NULL, &device_num);
		device_id = new cl_device_id[device_num];
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, device_num, device_id, NULL);
		if (k - device_count >= device_num)
			device_count += device_num;
		else
			return device_id[k - device_count];
		delete[] device_id;
	}
	delete[] platform_id;
	return nullptr;
}




void kernelManager::getDeviceFullInfo(int device_index)
{
	int strlen = 1024;
	char* buffer = new char[strlen];

	cl_device_id device = getDeviceID(device_index);
	if (device == nullptr)errorHandle("The given device is not found, please check if you have an opencl-enable device available!");
	cl_device_type type;
	clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(type), &type, NULL);
	if (type & CL_DEVICE_TYPE_DEFAULT) printf("CL_DEVICE_TYPE: %s\n", "CL_DEVICE_TYPE_DEFAULT");
	if (type & CL_DEVICE_TYPE_CPU) printf("CL_DEVICE_TYPE: %s\n", "CL_DEVICE_TYPE_CPU");
	if (type & CL_DEVICE_TYPE_GPU) printf("CL_DEVICE_TYPE: %s\n", "CL_DEVICE_TYPE_GPU");
	if (type & CL_DEVICE_TYPE_ACCELERATOR) printf("CL_DEVICE_TYPE: %s\n", "CL_DEVICE_TYPE_ACCELERATOR");
	if (type & CL_DEVICE_TYPE_CUSTOM) printf("CL_DEVICE_TYPE: %s\n", "CL_DEVICE_TYPE_CUSTOM");
	(clGetDeviceInfo(device, CL_DEVICE_NAME, strlen, buffer, NULL));
	printf("CL_DEVICE_NAME: %s\n", buffer);
	(clGetDeviceInfo(device, CL_DEVICE_VENDOR, strlen, buffer, NULL));
	printf("CL_DEVICE_VENDOR: %s\n", buffer);
	(clGetDeviceInfo(device, CL_DEVICE_OPENCL_C_VERSION, strlen, buffer, NULL));
	printf("CL_DEVICE_OPENCL_C_VERSION: %s\n", buffer);
	cl_uint max_compute_units;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(max_compute_units), &max_compute_units, NULL));
	printf("CL_DEVICE_MAX_COMPUTE_UNITS: %u\n", max_compute_units);
	cl_uint global_mem_cacheline_size;
	(clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE, sizeof(global_mem_cacheline_size), &global_mem_cacheline_size, NULL));
	printf("CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE: %u B\n", global_mem_cacheline_size);
	cl_ulong global_mem_cache_size;
	(clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE, sizeof(global_mem_cache_size), &global_mem_cache_size, NULL));
	printf("CL_DEVICE_GLOBAL_MEM_CACHE_SIZE: %llu B = %llu KB\n", global_mem_cache_size, global_mem_cache_size / 1024);
	cl_ulong global_mem_size;
	(clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(global_mem_size), &global_mem_size, NULL));
	printf("CL_DEVICE_GLOBAL_MEM_SIZE: %llu B = %llu MB\n", global_mem_size, global_mem_size / 1048576);
	cl_ulong max_constant_buffer_size;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(max_constant_buffer_size), &max_constant_buffer_size, NULL));
	printf("CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE: %llu B = %llu KB\n", max_constant_buffer_size, max_constant_buffer_size / 1024);
	cl_uint max_constant_args;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_ARGS, sizeof(max_constant_args), &max_constant_args, NULL));
	printf("CL_DEVICE_MAX_CONSTANT_ARGS: %u\n", max_constant_args);
	cl_device_local_mem_type local_mem_type;
	(clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_TYPE, sizeof(local_mem_type), &local_mem_type, NULL));
	if (local_mem_type == CL_NONE) printf("CL_DEVICE_LOCAL_MEM_TYPE: %s\n", "CL_NONE");
	if (local_mem_type == CL_LOCAL) printf("CL_DEVICE_LOCAL_MEM_TYPE: %s\n", "CL_LOCAL");
	if (local_mem_type == CL_GLOBAL) printf("CL_DEVICE_LOCAL_MEM_TYPE: %s\n", "CL_GLOBAL");
	cl_ulong local_mem_size;
	(clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(local_mem_size), &local_mem_size, NULL));
	printf("CL_DEVICE_LOCAL_MEM_SIZE: %llu B = %llu KB\n", local_mem_size, local_mem_size / 1024);
	cl_bool host_unified_memory;
	(clGetDeviceInfo(device, CL_DEVICE_HOST_UNIFIED_MEMORY, sizeof(host_unified_memory), &host_unified_memory, NULL));
	printf("CL_DEVICE_HOST_UNIFIED_MEMORY: %u\n", host_unified_memory);
	cl_uint max_work_item_dimensions;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(max_work_item_dimensions), &max_work_item_dimensions, NULL));
	printf("CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS: %u\n", max_work_item_dimensions);
	size_t* max_work_item_sizes = (size_t*)malloc(sizeof(size_t) * max_work_item_dimensions);
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(size_t) * max_work_item_dimensions, max_work_item_sizes, NULL));
	printf("CL_DEVICE_MAX_WORK_ITEM_SIZES: "); for (size_t i = 0; i < max_work_item_dimensions; ++i) printf("%zu\t", max_work_item_sizes[i]); printf("\n");
	free(max_work_item_sizes);
	size_t max_work_group_size;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(max_work_group_size), &max_work_group_size, NULL));
	printf("CL_DEVICE_MAX_WORK_GROUP_SIZE: %lu\n", max_work_group_size);
	delete[] buffer;
}

