#pragma once
#include "R_ext/libextern.h"
#include <R.h>
#include <Rinternals.h>


//extern "C" LibExport
SEXP getPlatformNum();
//extern "C" LibExport
SEXP getDeviceNum(SEXP platform);
//extern "C" LibExport
SEXP getDeviceInfo(SEXP platform, SEXP device);

//char** deviceName,int* deviceType,double* global_memory,
//double* local_memory, int* haslocalMemory, char** opencl_version, int* compute_unit_num

//extern "C" LibExport
SEXP upload(SEXP platform, SEXP deviceNum, SEXP data, SEXP length, SEXP type);
//extern "C" LibExport
SEXP gpuMalloc(SEXP platform, SEXP deviceNum, SEXP length, SEXP type);

//extern "C" LibExport
SEXP download(SEXP address);


//extern "C" LibExport
SEXP release(SEXP address);
//extern "C" LibExport
SEXP hasKernel(SEXP platform, SEXP deviceNum, SEXP signature, SEXP kernel);

//extern "C" LibExport
SEXP createKernel(SEXP platform, SEXP deviceNum, SEXP signature, SEXP flag, SEXP code, SEXP kernel);
//extern "C" LibExport
SEXP setParameter(SEXP platform, SEXP deviceNum, SEXP signature, SEXP kernel, SEXP data_address, SEXP parm_index);
//extern "C" LibExport
SEXP setSharedParameter(SEXP platform, SEXP deviceNum, SEXP signature, SEXP kernel, SEXP size, SEXP parm_index);
//extern "C" LibExport
SEXP launchKernel(SEXP platform, SEXP deviceNum, SEXP signature, SEXP kernel, SEXP blockSize, SEXP threadSize);

//extern "C" LibExport
SEXP getPreferredGroupSize(SEXP platform, SEXP deviceNum, SEXP signature, SEXP kernel);
//extern "C" LibExport
SEXP getDeviceStatus(SEXP platform, SEXP deviceNum);

//Get the address and return it to R in double format
//extern "C" LibExport
SEXP getTrueAd(SEXP ad);

////extern "C" LibExport
//SEXP test();

SEXP asMatrix(SEXP data, SEXP dim);