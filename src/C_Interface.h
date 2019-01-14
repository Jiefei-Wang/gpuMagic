#pragma once
#include "R_ext/libextern.h"
#include <R.h>
#include <Rinternals.h>


extern "C" LibExport
void getPlatformNum(int* platformNum);
extern "C" LibExport
void getDeviceNum(int* platform, int* deviceNum);
extern "C" LibExport
void getDeviceInfo(int* platform, int* device,
	char** deviceName,int* deviceType,double* global_memory,
	double* local_memory,int* haslocalMemory,char** opencl_version,int* compute_unit_num);



extern "C" LibExport
SEXP upload(SEXP platform, SEXP deviceNum, SEXP data, SEXP length, SEXP type);
extern "C" LibExport
void gpuMalloc(int* platform, int* deviceNum,double* length, int* type, void** address);

extern "C" LibExport
void download(void* data, void** address);


extern "C" LibExport
void release(void** address);
extern "C" LibExport
void hasKernel(int* platform, int* deviceNum,char** signature, char** kernel, bool* res);

extern "C" LibExport
void createKernel(int* platform, int* deviceNum,char** signature, char** flag, char** code, char** kernel);
extern "C" LibExport
void setParameter(int* platform, int* deviceNum, char** signature, char** kernel, void** data_address, int *parm_index);
extern "C" LibExport
void setSharedParameter(int* platform, int* deviceNum, char** signature, char** kernel, int* size, int *parm_index);
extern "C" LibExport
void launchKernel(int* platform, int* deviceNum, char** signature, char** kernel, int* blockSize, int* threadSize);

extern "C" LibExport
void getPreferredGroupSize(int* platform, int* deviceNum, char** signature, char** kernel, double* res);
extern "C" LibExport
void getDeviceStatus(int* platform, int* deviceNum, int* status);

extern "C" LibExport
void getDeviceFullInfo(int* platform, int* deviceNum);

//extern "C" LibExport
//SEXP test();