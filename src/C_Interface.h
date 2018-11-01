#pragma once
#include "R_ext/libextern.h"


extern "C" LibExport
void getCurDeviceIndex(int* id);

extern "C" LibExport
void upload(void* data, double * dim, int* type, void** address);
extern "C" LibExport
void download(void* data, void** address);

extern "C" LibExport
void clear(void** address);
extern "C" LibExport
void hasKernel(char** signature, char** kernel, bool* res);

extern "C" LibExport
void createKernel(char** signature,char** kernel,  char** code);
extern "C" LibExport
void loadParameter(char** signature, char** kernel, void** data_address, int *parm_index);
extern "C" LibExport
void loadSharedParameter(char** signature, char** kernel, int* size, int *parm_index);
extern "C" LibExport
void launchKernel(char** signature, char** kernel, int* blockSize, int* threadSize);



extern "C" LibExport
void getDeviceList();
extern "C" LibExport
void getDeviceInfo(int * i);
extern "C" LibExport
void getDeviceDetail(int * i);
extern "C" LibExport
void setDevice(int * i);
extern "C" LibExport
void getCurDevice();

extern "C" LibExport
void getDeviceSharedMem(int* id, double* mem);

extern "C" LibExport
void debug(bool* test, int* length);