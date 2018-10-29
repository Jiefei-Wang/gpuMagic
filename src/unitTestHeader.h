#pragma once
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "R_ext/libextern.h"
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include "Tools.h"
#include "kernelManager.h"
#include "openArray.h"
#include "C_Interface.h"
#include <string> 
using namespace std;
