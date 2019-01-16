
#include <R_ext/RS.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "C_Interface.h"


static const R_CallMethodDef callMethods[] = {
	{ "getPlatformNum", (DL_FUNC)& getPlatformNum, 0 },
	{ "getDeviceNum", (DL_FUNC)& getDeviceNum, 1 },
	{ "getDeviceInfo", (DL_FUNC)& getDeviceInfo, 2 },
	{ "upload", (DL_FUNC)& upload, 5 },
	{ "gpuMalloc", (DL_FUNC)& gpuMalloc, 4 },
	{ "download", (DL_FUNC)& download, 1 },
	{ "release", (DL_FUNC)& release, 1 },
	{ "hasKernel", (DL_FUNC)& hasKernel, 4 },
	{ "createKernel", (DL_FUNC)& createKernel, 6 },
	{ "setParameter", (DL_FUNC)& setParameter, 6 },
	{ "setSharedParameter", (DL_FUNC)& setSharedParameter, 6 },
	{ "launchKernel", (DL_FUNC)& launchKernel, 6 },
	{ "getPreferredGroupSize", (DL_FUNC)& getPreferredGroupSize, 4 },
	{ "getDeviceStatus", (DL_FUNC)& getDeviceStatus, 2 },
	{ "getTrueAd", (DL_FUNC)& getTrueAd, 1 },
	{ "asMatrix", (DL_FUNC)& asMatrix, 2 },
	{ NULL, NULL, 0 }
};

extern "C"
void R_init_gpuMagic(DllInfo * info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}