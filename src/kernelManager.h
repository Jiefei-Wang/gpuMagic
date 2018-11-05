#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <map>
#include <iostream>

#include <fstream>
#include<vector>
using namespace std;

class kernelManager {
public:
	static string compiler_flag;

	static int deviceIndex;
	static cl_context context;
	static cl_device_id device_id;
	static cl_command_queue command_queue;

	static  map< int, cl_context> contextTable;
	static  map< int, cl_device_id> deviceTable;
	static  map< int, cl_command_queue> queueTable;
	static  map< string, cl_program> programTable;
	static  map< string, cl_kernel> kernelTable;
public:
	//Print out all device name
	static void getAllDeviceName();
	//Print out the current device compatibility
	static void getDeviceInfo(int device_index);
	static void getDeviceFullInfo(int device);
	static void getCurDevice();

	//kernel setting functions
	static void selectDevice(int device);
	static void destroyDevice(int device);
	static int getDeviceIndex();
	static bool hasKernel( string signature,  string kernel);
	static cl_kernel getKernel( string signature,  string kernel);
	static cl_kernel createKernel( string signature,  string kernel,  string code);

	//Resources access
	static cl_context getContext(int device);
	static cl_device_id getDevice(int device);
	static cl_command_queue getQueue(int device);
	static void checkAndInitializeManager();
private:
	static cl_program loadProgram( string signature,  string code);
	static void initializeDevice(int device=0);
	static cl_device_id getDeviceID(int k);
	static string getSignature(string sig, string kernel);
};
