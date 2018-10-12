#include "kernelManager.h"
#include "Tools.h"
#include <string> 
using namespace std;
int kernelManager::deviceIndex = 0;
cl_context kernelManager::context = nullptr;
cl_device_id kernelManager::device_id = nullptr;
cl_command_queue kernelManager::command_queue = nullptr;
std::map<std::string, cl_program> kernelManager::programTable;
std::map<std::string, cl_kernel> kernelManager::kernelTable;



 


void kernelManager::getAllDeviceName() {
	cl_uint platform_num;
	clGetPlatformIDs(0, NULL, &platform_num);
	cl_platform_id* platform_id = new cl_platform_id[platform_num];
	clGetPlatformIDs(platform_num, platform_id, NULL);
	cl_uint device_num;
	cl_device_id* device_id;
	int device_count = 0;
	for (int i = 0; i < platform_num; i++) {
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, NULL, NULL, &device_num);
		device_id = new cl_device_id[device_num];
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, device_num, device_id, NULL);
		size_t name_size;
		char* device_name;
		for (int j = 0; j < device_num; j++) {
			clGetDeviceInfo(device_id[j], CL_DEVICE_NAME, NULL, NULL, &name_size);
			device_name = new char[name_size];
			clGetDeviceInfo(device_id[j], CL_DEVICE_NAME, name_size, device_name, NULL);
			string info = string("Device ") + std::to_string(device_count) + ": " + device_name;
			message(info);
			device_count++;
			delete[] device_name;
		}
		delete[] device_id;
	}
	delete[] platform_id;
	if (platform_num == 0) message("No device is available, do you forget to install the driver?") ;
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
	printf("Device memory size: %lu MB\n", global_mem_size / 1048576);
	delete[] buffer;
}

void kernelManager::getCurDevice()
{
	getDeviceInfo(deviceIndex);
}

void kernelManager::setDevice(int device)
{
	cl_int error;
	destroyContext();
	device_id = getDeviceID(device);
	if (device_id == nullptr) errorHandle("The given device is not found, please check if you have an opencl-enable device available!");
	context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a context associated with the current device!");
	command_queue = clCreateCommandQueue(context, device_id, 0, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a command queue associated with the current device!");
	deviceIndex = device;
}

void kernelManager::destroyContext()
{
	if (context == nullptr) return;
	cl_int error;
	error = clFlush(command_queue);
	error = clFinish(command_queue);
	for (std::map<std::string, cl_kernel>::iterator it = kernelTable.begin(); it != kernelTable.end(); ++it) {
		error += clReleaseKernel(it->second);
	}
	for (std::map<std::string, cl_program>::iterator it = programTable.begin(); it != programTable.end(); ++it) {
		error += clReleaseProgram(it->second);
	}
	programTable.clear();
	kernelTable.clear();
	error += clReleaseCommandQueue(command_queue);
	error += clReleaseContext(context);
	command_queue = nullptr;
	context = nullptr;
	device_id = nullptr;
	if (error != CL_SUCCESS) errorHandle("An error has occured in releasing context");
}

bool kernelManager::hasKernel(std::string signature, std::string kernel)
{
	if (kernelTable.find(signature + kernel) != kernelTable.end())
		return true;
	else
		return false;
}



cl_kernel kernelManager::getKernel(std::string signature, string kernel)
{
	if (kernelTable.find(signature+ kernel) != kernelTable.end())
		return kernelTable[signature+ kernel];
	errorHandle("The given kernel does not find");
}

cl_kernel kernelManager::createKernel(std::string signature,string kernel,string code)
{
	if (kernelTable.find(signature+ kernel) != kernelTable.end())
		return kernelTable[signature+ kernel];
	cl_program program=loadProgram(signature+ kernel, code);
	cl_int error;
	cl_kernel dev_kernel = clCreateKernel(program, kernel.c_str(), &error);

	switch (error) {
	case 0:
		break;
	default:
		string errorInfo = string("Fail to create kernel, error info: ") + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
	}

	kernelTable.insert(make_pair(signature+ kernel, dev_kernel));
	return dev_kernel;
}


cl_program kernelManager::loadProgram(string signature,string code)
{
	if (context == nullptr)
		initializeManager();
	if (programTable.find(signature) != programTable.end())
		return programTable[signature];

	// create and build program
	cl_int error = 0;
	const char* source = code.c_str();
	//printf(source);
	cl_program program = clCreateProgramWithSource(context, 1, &source, 0, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to read program, error info: ") + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
		return NULL;
	}
	error = clBuildProgram(program, 1, &device_id, 0, 0, 0);
	
	switch (error) {
	case CL_SUCCESS:
		break;
	case CL_BUILD_PROGRAM_FAILURE: {
		int strlen = 1024;
		char* buffer = new char[strlen];
		// Get the log
		clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, strlen, buffer, NULL);
		// Print the log
		string errorInfo = string("Fail to build program, error info: \n") + string(buffer);
		errorHandle(errorInfo.c_str());
		delete[] buffer;
		break;
	}
	default: {
		string errorInfo = string("Fail to build program, error info: ") + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
	}
	}
	programTable.insert(make_pair(signature, program));
	return program;
}

cl_context kernelManager::getContext()
{
	if (context == nullptr)
		initializeManager();
	return context;
}

cl_device_id kernelManager::getDevice()
{
	if (device_id == nullptr)
		initializeManager();
	return device_id;
}

cl_command_queue kernelManager::getQueue()
{
	if (command_queue == nullptr)
		initializeManager();
	return command_queue;
}



void kernelManager::initializeManager()
{
	setDevice(deviceIndex);
}



//======================Some lengthy functions==============================
cl_device_id kernelManager::getDeviceID(int k)
{
	cl_uint platform_num;
	clGetPlatformIDs(0, NULL, &platform_num);
	cl_platform_id* platform_id = new cl_platform_id[platform_num];
	clGetPlatformIDs(platform_num, platform_id, NULL);
	cl_uint device_num;
	cl_device_id* device_id;
	int device_count = 0;
	for (int i = 0; i < platform_num; i++) {
		clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, NULL, NULL, &device_num);
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
	char* buffer=new char[strlen];

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
	printf("CL_DEVICE_GLOBAL_MEM_CACHE_SIZE: %lu B = %lu KB\n", global_mem_cache_size, global_mem_cache_size / 1024);
	cl_ulong global_mem_size;
	(clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(global_mem_size), &global_mem_size, NULL));
	printf("CL_DEVICE_GLOBAL_MEM_SIZE: %lu B = %lu MB\n", global_mem_size, global_mem_size / 1048576);
	cl_ulong max_constant_buffer_size;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(max_constant_buffer_size), &max_constant_buffer_size, NULL));
	printf("CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE: %lu B = %lu KB\n", max_constant_buffer_size, max_constant_buffer_size / 1024);
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
	printf("CL_DEVICE_LOCAL_MEM_SIZE: %lu B = %lu KB\n", local_mem_size, local_mem_size / 1024);
	cl_bool host_unified_memory;
	(clGetDeviceInfo(device, CL_DEVICE_HOST_UNIFIED_MEMORY, sizeof(host_unified_memory), &host_unified_memory, NULL));
	printf("CL_DEVICE_HOST_UNIFIED_MEMORY: %u\n", host_unified_memory);
	cl_uint max_work_item_dimensions;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(max_work_item_dimensions), &max_work_item_dimensions, NULL));
	printf("CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS: %u\n", max_work_item_dimensions);
	size_t* max_work_item_sizes = (size_t*)malloc(sizeof(size_t) * max_work_item_dimensions);
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(size_t) * max_work_item_dimensions, max_work_item_sizes, NULL));
	printf("CL_DEVICE_MAX_WORK_ITEM_SIZES: "); for (size_t i = 0; i < max_work_item_dimensions; ++i) printf("%lu\t", max_work_item_sizes[i]); printf("\n");
	free(max_work_item_sizes);
	size_t max_work_group_size;
	(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(max_work_group_size), &max_work_group_size, NULL));
	printf("CL_DEVICE_MAX_WORK_GROUP_SIZE: %lu\n", max_work_group_size);
	delete[] buffer;
}


const char *kernelManager::getErrorString(cl_int error)
{
	switch (error) {
		// run-time and JIT compiler errors
	case 0: return "CL_SUCCESS";
	case -1: return "CL_DEVICE_NOT_FOUND";
	case -2: return "CL_DEVICE_NOT_AVAILABLE";
	case -3: return "CL_COMPILER_NOT_AVAILABLE";
	case -4: return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
	case -5: return "CL_OUT_OF_RESOURCES";
	case -6: return "CL_OUT_OF_HOST_MEMORY";
	case -7: return "CL_PROFILING_INFO_NOT_AVAILABLE";
	case -8: return "CL_MEM_COPY_OVERLAP";
	case -9: return "CL_IMAGE_FORMAT_MISMATCH";
	case -10: return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
	case -11: return "CL_BUILD_PROGRAM_FAILURE";
	case -12: return "CL_MAP_FAILURE";
	case -13: return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
	case -14: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
	case -15: return "CL_COMPILE_PROGRAM_FAILURE";
	case -16: return "CL_LINKER_NOT_AVAILABLE";
	case -17: return "CL_LINK_PROGRAM_FAILURE";
	case -18: return "CL_DEVICE_PARTITION_FAILED";
	case -19: return "CL_KERNEL_ARG_INFO_NOT_AVAILABLE";

		// compile-time errors
	case -30: return "CL_INVALID_VALUE";
	case -31: return "CL_INVALID_DEVICE_TYPE";
	case -32: return "CL_INVALID_PLATFORM";
	case -33: return "CL_INVALID_DEVICE";
	case -34: return "CL_INVALID_CONTEXT";
	case -35: return "CL_INVALID_QUEUE_PROPERTIES";
	case -36: return "CL_INVALID_COMMAND_QUEUE";
	case -37: return "CL_INVALID_HOST_PTR";
	case -38: return "CL_INVALID_MEM_OBJECT";
	case -39: return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
	case -40: return "CL_INVALID_IMAGE_SIZE";
	case -41: return "CL_INVALID_SAMPLER";
	case -42: return "CL_INVALID_BINARY";
	case -43: return "CL_INVALID_BUILD_OPTIONS";
	case -44: return "CL_INVALID_PROGRAM";
	case -45: return "CL_INVALID_PROGRAM_EXECUTABLE";
	case -46: return "CL_INVALID_KERNEL_NAME";
	case -47: return "CL_INVALID_KERNEL_DEFINITION";
	case -48: return "CL_INVALID_KERNEL";
	case -49: return "CL_INVALID_ARG_INDEX";
	case -50: return "CL_INVALID_ARG_VALUE";
	case -51: return "CL_INVALID_ARG_SIZE";
	case -52: return "CL_INVALID_KERNEL_ARGS";
	case -53: return "CL_INVALID_WORK_DIMENSION";
	case -54: return "CL_INVALID_WORK_GROUP_SIZE";
	case -55: return "CL_INVALID_WORK_ITEM_SIZE";
	case -56: return "CL_INVALID_GLOBAL_OFFSET";
	case -57: return "CL_INVALID_EVENT_WAIT_LIST";
	case -58: return "CL_INVALID_EVENT";
	case -59: return "CL_INVALID_OPERATION";
	case -60: return "CL_INVALID_GL_OBJECT";
	case -61: return "CL_INVALID_BUFFER_SIZE";
	case -62: return "CL_INVALID_MIP_LEVEL";
	case -63: return "CL_INVALID_GLOBAL_WORK_SIZE";
	case -64: return "CL_INVALID_PROPERTY";
	case -65: return "CL_INVALID_IMAGE_DESCRIPTOR";
	case -66: return "CL_INVALID_COMPILER_OPTIONS";
	case -67: return "CL_INVALID_LINKER_OPTIONS";
	case -68: return "CL_INVALID_DEVICE_PARTITION_COUNT";

		// extension errors
	case -1000: return "CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR";
	case -1001: return "CL_PLATFORM_NOT_FOUND_KHR";
	case -1002: return "CL_INVALID_D3D10_DEVICE_KHR";
	case -1003: return "CL_INVALID_D3D10_RESOURCE_KHR";
	case -1004: return "CL_D3D10_RESOURCE_ALREADY_ACQUIRED_KHR";
	case -1005: return "CL_D3D10_RESOURCE_NOT_ACQUIRED_KHR";
	default: return "Unknown OpenCL error";
	}
}
