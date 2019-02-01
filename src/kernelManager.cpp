#include "kernelManager.h"

map< deviceIdentifier, deviceContext> kernelManager::deviceTable;

map< deviceIdentifier, map< string, cl_program>> kernelManager::device_programTable;
map< deviceIdentifier, map< string, cl_kernel>> kernelManager::device_kernelTable;


unsigned int kernelManager::getPlatformNum()
{
	cl_uint platform_num;
	clGetPlatformIDs(0, NULL, &platform_num);
	return platform_num;
}

unsigned int kernelManager::getDeviceNum(unsigned int platform_id)
{
	cl_platform_id platform = getPlatformId(platform_id);
	cl_uint device_num;
	clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, NULL, &device_num);

	return device_num;
}
cl_platform_id kernelManager::getPlatformId(unsigned int platform_id)
{
	unsigned int platform_num = getPlatformNum();
	if (platform_num <= platform_id) errorHandle("Invalid platform id");
	cl_platform_id* platform_ids = new cl_platform_id[platform_num];
	clGetPlatformIDs(platform_num, platform_ids, NULL);
	cl_platform_id platform = platform_ids[platform_id];

	delete[] platform_ids;
	return platform;
}

cl_device_id kernelManager::getDeviceId(deviceIdentifier deviceId)
{
	cl_platform_id platform = getPlatformId(deviceId.platform_id);
	cl_uint device_num = getDeviceNum(deviceId.platform_id);
	if ((int)device_num <= deviceId.device_id)errorHandle("Invalid device id");
	cl_device_id* device_ids = new cl_device_id[device_num];
	clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, device_num, device_ids, NULL);
	cl_device_id device = device_ids[deviceId.device_id];

	delete[] device_ids;
	return device;
}

deviceInfo kernelManager::getDeviceInfo(deviceIdentifier deviceId)
{
	deviceInfo info;

	cl_device_id device = getDeviceId(deviceId);

	int strlen = 1024;
	char* buffer = new char[strlen];

	info.platform_id = deviceId.platform_id;
	info.device_id = deviceId.device_id;

	clGetDeviceInfo(device, CL_DEVICE_NAME, strlen, buffer, NULL);
	info.device_name = string(buffer);

	cl_device_type device_type;
	clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(cl_device_type), &device_type, NULL);
	switch (device_type) {
	case CL_DEVICE_TYPE_CPU:
		info.device_type = 0;
		break;
	case CL_DEVICE_TYPE_GPU:
		info.device_type = 1;
		break;
	default:
		info.device_type = 2;
	}
	cl_ulong size_var;
	clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(cl_ulong), &size_var, NULL);
	info.global_memory = size_var;

	clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(cl_ulong), &size_var, NULL);
	info.local_memory = size_var;

	cl_device_local_mem_type local_type;
	clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_TYPE, sizeof(cl_device_local_mem_type), &local_type, NULL);
	if (local_type == CL_LOCAL)
		info.has_local_memory = true;
	else
		info.has_local_memory = false;

	clGetDeviceInfo(device, CL_DEVICE_OPENCL_C_VERSION, strlen, buffer, NULL);
	info.opencl_version = string(buffer);


	cl_uint cu;
	clGetDeviceInfo(device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(cl_uint), &cu, NULL);
	info.compute_unit_num = cu;

	delete[] buffer;
	return info;
}

void kernelManager::initializeDevice(deviceIdentifier deviceId)
{
	if (hasDevice(deviceId)) {
		return;
	}
	//If the device_id is not found, create the device_id
	cl_int error;
	cl_device_id curDevice_id = getDeviceId(deviceId);
	if (curDevice_id == nullptr) errorHandle("The given device is not found, please check if you have an opencl-enable device available!");
	cl_context curContext = clCreateContext(NULL, 1, &curDevice_id, NULL, NULL, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a context associated with the current device!");
	cl_command_queue curCommand_queue = clCreateCommandQueue(curContext, curDevice_id, 0, &error);
	if (error != CL_SUCCESS) errorHandle("Cannot create a command queue associated with the current device!");
	//cl_event curEvent;
	deviceContext curDeviceInfo = { curContext ,curDevice_id ,curCommand_queue , cl_event() };
	map< string, cl_kernel> curKernalTable;
	map< string, cl_program> curProgramTable;
	//Insert the device related objects to the table
	deviceTable[deviceId] = curDeviceInfo;
	device_kernelTable[deviceId] = curKernalTable;
	device_programTable[deviceId] = curProgramTable;

}

bool kernelManager::hasDevice(deviceIdentifier deviceId)
{
	if (deviceTable.find(deviceId) == deviceTable.end())
		return false;
	else
		return true;
}

deviceContext& kernelManager::getDevice(deviceIdentifier deviceId)
{
	initializeDevice(deviceId);
	return(deviceTable[deviceId]);
}


void kernelManager::destroyDevice(deviceIdentifier deviceId)
{
	if (hasDevice(deviceId))
		return;
	deviceContext device = getDevice(deviceId);
	cl_int error;
	error = clFlush(device.command_queue);
	error += clFinish(device.command_queue);

	map< string, cl_kernel>& kernelTable = getKernelTable(deviceId);
	map< string, cl_program>& programTable = getProgramTable(deviceId);

	for (map< string, cl_kernel>::iterator it = kernelTable.begin(); it != kernelTable.end(); ++it) {
		error += clReleaseKernel(it->second);
	}
	for (map< string, cl_program>::iterator it = programTable.begin(); it != programTable.end(); ++it) {
		error += clReleaseProgram(it->second);
	}
	programTable.clear();
	kernelTable.clear();
	error += clReleaseCommandQueue(device.command_queue);
	error += clReleaseContext(device.context);

	deviceTable.erase(deviceId);
	device_programTable.erase(deviceId);
	device_kernelTable.erase(deviceId);

	if (error != CL_SUCCESS) errorHandle("An error has occured during releasing the device resources");
}


bool kernelManager::hasKernel(deviceIdentifier deviceId, programSignature programSig)
{
	if (!hasDevice(deviceId))
		return false;
	map< string, cl_kernel>& kernelTable = getKernelTable(deviceId);
	if (kernelTable.find(programSig.signature+ programSig.kernel) != kernelTable.end())
		return true;
	else
		return false;
}

cl_kernel kernelManager::getKernel(deviceIdentifier deviceId, programSignature programSig)
{
	if (hasKernel(deviceId, programSig)) {
		map< string, cl_kernel>& kernelTable = getKernelTable(deviceId);
		return kernelTable[programSig.signature+ programSig.kernel];
	}
	errorHandle("The given kernel does not find");
	return NULL;
}

cl_kernel kernelManager::createKernel(deviceIdentifier deviceId, programSignature programSig)
{
	if (hasKernel(deviceId, programSig))
		return getKernel(deviceId, programSig);

	map< string, cl_kernel>& kernelTable = getKernelTable(deviceId);
	cl_program program = createProgram(deviceId, programSig);
	cl_int error;
	cl_kernel dev_kernel = clCreateKernel(program, programSig.kernel.c_str(), &error);

	switch (error) {
	case 0:
		break;
	default:
		string errorInfo = string("Fail to create kernel, error info: ") + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
	}

	kernelTable.insert(make_pair(programSig.signature+ programSig.kernel, dev_kernel));
	return dev_kernel;
}

bool kernelManager::hasProgram(deviceIdentifier deviceId, programSignature programSig)
{

	if (!hasDevice(deviceId))
		return false;
	map< string, cl_program>& programTable = getProgramTable(deviceId);
	//Remove the kernel name since it is not a part of program key
	programSig.kernel = "";
	if (programTable.find(programSig.signature) != programTable.end())
		return true;
	else
		return false;
}

cl_program kernelManager::getProgram(deviceIdentifier deviceId, programSignature programSig)
{
	if (!hasProgram(deviceId, programSig)) {
		errorHandle("The given program does not find");
		return NULL;
	}
	map< string, cl_program>& programTable = getProgramTable(deviceId);
	return programTable[programSig.signature];
}


cl_program kernelManager::createProgram(deviceIdentifier deviceId, programSignature programSig)
{
	if (hasProgram(deviceId, programSig))
		return getProgram(deviceId, programSig);

	// create and build program
	//Make sure the device exist
	deviceContext dc = getDevice(deviceId);
	map< string, cl_program>& programTable = getProgramTable(deviceId);
	cl_int error = 0;
	const char* source = programSig.code.c_str();
	//printf(source);
	cl_program program = clCreateProgramWithSource(dc.context, 1, &source, 0, &error);
	if (error != CL_SUCCESS) {
		string errorInfo = string("Fail to read program, error info: ") + to_string(error) + "-" + string(getErrorString(error));
		errorHandle(errorInfo.c_str());
		return NULL;
	}
	message(programSig.compiler_flag.c_str());
	error = clBuildProgram(program, 1, &dc.device_id, programSig.compiler_flag.c_str(), 0, 0);
	switch (error) {
	case CL_SUCCESS:
		break;
	case CL_BUILD_PROGRAM_FAILURE: {
		size_t strlen = 0;
		// Get the log
		clGetProgramBuildInfo(program, dc.device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &strlen);
		char *buffer = new char[strlen];
		clGetProgramBuildInfo(program, dc.device_id, CL_PROGRAM_BUILD_LOG, strlen, buffer, NULL);
		// Print the log
		string errorInfo = string("Fail to build program: ") + to_string(error) + "\n" + string(buffer);
		errorHandle(errorInfo);
		delete[](buffer);

	}
	default: {
		string errorInfo = string("Fail to build program: ") + to_string(error) + "-" + string(getErrorString(error));
		errorHandle(errorInfo);
	}
	}
	//Remove the kernel name before insert the program into the map
	programSig.kernel = "";
	programTable.insert(make_pair(programSig.signature, program));
	return program;
}





map<string, cl_program>& kernelManager::getProgramTable(deviceIdentifier device)
{
	initializeDevice(device);
	if (device_programTable.find(device) == device_programTable.end()) {
		errorHandle("Unable to find the program table!");
	}
	return device_programTable[device];
}

map<string, cl_kernel>& kernelManager::getKernelTable(deviceIdentifier device)
{
	initializeDevice(device);
	if (device_kernelTable.find(device) == device_kernelTable.end()) {
		errorHandle("Unable to find the kernel table!\nPlatform id:"+ std::to_string(device.platform_id)+" Device id:"+ std::to_string(device.device_id));
	}
	return device_kernelTable[device];
}






bool kernelManager::isCurDeviceValid(deviceIdentifier deviceId)
{
	if (deviceId.device_id >= 0)
		return true;
	return false;
}
