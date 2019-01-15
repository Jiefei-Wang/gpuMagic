#pragma once
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <map>
#include <iostream>

#include<vector>
#include <string> 
#include "Tools.h"

using namespace std;
struct deviceIdentifier;
struct deviceContext;
struct programSignature;
struct deviceInfo;

class kernelManager {
public:

	static  map< deviceIdentifier, deviceContext> deviceTable;


	static  map< deviceIdentifier, map< string, cl_program>> device_programTable;
	static  map< deviceIdentifier, map< string, cl_kernel>> device_kernelTable;

public:
	//get number of platforms or devices
	static unsigned int getPlatformNum();
	static unsigned int getDeviceNum(unsigned int platform_id);
	//get a particular platform or device
	static cl_platform_id getPlatformId(unsigned int platform_id);
	static cl_device_id getDeviceId(deviceIdentifier deviceId);
	//get device information
	static deviceInfo getDeviceInfo(deviceIdentifier deviceId);

	//Device operation
	//The function is safe to call
	static void initializeDevice(deviceIdentifier deviceId);
	static bool hasDevice(deviceIdentifier deviceId);
	static deviceContext& getDevice(deviceIdentifier deviceId);
	static void destroyDevice(deviceIdentifier deviceInd);

	//kernel operation
	static bool hasKernel(deviceIdentifier deviceId, programSignature programSig);
	//Will throw exception when the kernel does not exist
	static cl_kernel getKernel(deviceIdentifier deviceId, programSignature programSig);
	static cl_kernel createKernel(deviceIdentifier deviceId, programSignature programSig);
	//program operation
	static bool hasProgram(deviceIdentifier deviceId, programSignature programSig);
	//Will throw exception when the kernel does not exist
	static cl_program getProgram(deviceIdentifier deviceId, programSignature programSig);
	static cl_program createProgram(deviceIdentifier deviceId, programSignature programSig);


private:
	static map< string, cl_program>& getProgramTable(deviceIdentifier deviceId);
	static map< string, cl_kernel>& getKernelTable(deviceIdentifier deviceId);
	//Check if the current device is valid or not
	static bool isCurDeviceValid(deviceIdentifier deviceId);
};


struct deviceInfo
{
	unsigned int platform_id;
	unsigned int device_id;
	std::string device_name;
	//0:cpu,1.gpu,2.other
	int device_type;
	unsigned long long global_memory;
	unsigned long long local_memory;
	bool has_local_memory;
	std::string opencl_version;
	unsigned int compute_unit_num;
};
//This struct storage the device and platform id in numeric form
struct deviceIdentifier {
	int platform_id;
	int device_id;
	bool operator == (const deviceIdentifier &rhs) const {
		return this->device_id == rhs.device_id&&this->platform_id == rhs.platform_id;
	}
	bool operator<(const deviceIdentifier &rhs)  const {
		int k = 100;
		return platform_id * k + device_id < rhs.platform_id*k + rhs.device_id;
	}
};
//This object storage all the device property
struct deviceContext
{
	cl_context context;
	cl_device_id device_id;
	cl_command_queue command_queue;
	cl_event queue_event;
};

struct programSignature {
	string signature;
	string compiler_flag;
	string code;
	string kernel;
	bool operator<(const programSignature &rhs)  const {
		return signature + compiler_flag + code + kernel < rhs.signature + rhs.compiler_flag + rhs.code + rhs.kernel;
	}
};
