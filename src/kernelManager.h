#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <map>
#include <iostream>

#include <fstream>
#include<vector>

class kernelManager {
public:
	static int deviceIndex;
	static cl_context context;
	static cl_device_id device_id;
	static cl_command_queue command_queue;
	static std::map<std::string, cl_program> programTable;
	static std::map<std::string, cl_kernel> kernelTable;
public:
	//Print out all device name
	static void getAllDeviceName();
	//Print out the current device compatibility
	static void getDeviceInfo(int device_index);
	static void getDeviceFullInfo(int device);
	static void getCurDevice();
	static const char* getErrorString(cl_int error);

	//kernel setting functions
	static void setDevice(int device);
	static void destroyContext();
	static bool hasKernel(std::string signature, std::string kernel);
	static cl_kernel getKernel(std::string signature, std::string kernel);
	static cl_kernel createKernel(std::string signature, std::string kernel, std::string code);

	//Resources access
	static cl_context getContext();
	static cl_device_id getDevice();
	static cl_command_queue getQueue();
private:
	static cl_program loadProgram(std::string signature, std::string code);
	static void initializeManager();
	static cl_device_id getDeviceID(int k);
};
