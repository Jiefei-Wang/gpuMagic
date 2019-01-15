
#include "unitTestHeader.h"
void test1();
int main(void) {
	int p = 0;
	int d = 0;
	int s = 0;
	//getDeviceStatus(&p, &d, &s);
	/*
	int platform = 1;
	int device = 0;
	deviceIdentifier deviceid = { platform,device };
	char* src = "kernel void kernelAdd(global double* a,global int* b,global double* c){\n size_t i=get_global_id(0);\nc[i]=a[i]+b[i];}";
	char* kernel = "kernelAdd";
	char* sig = "a";
	char* flag = "";
	int n = 20;
	double* data1 = new double[n];
	int* data2 = new int[n];
	for (int i = 0; i < n; i++) {
		data1[i] = i;
		data2[i] = 2 * i;
	}
	openArray d1(deviceid, data1, n, dtype::f64);
	openArray d2(deviceid, data2, n, dtype::i32);
	openArray* d3 = openArray::constant(deviceid, 0, n, dtype::f64);
	createKernel(&platform, &device, &sig, &flag, &src, &kernel);
	int id = 0;
	void * ad = (void*)&d1;
	setParameter(&platform, &device, &sig, &kernel, &ad, &id);
	id = 1;
	ad = (void*)&d2;
	setParameter(&platform, &device, &sig, &kernel, &ad, &id);
	id = 2;
	ad = (void*)d3;
	setParameter(&platform, &device, &sig, &kernel, &ad, &id);
	int block = n;
	int thread = 1;
	launchKernel(&platform, &device, &sig, &kernel, &block, &thread);
	int status=-1;
	getDeviceStatus(&platform, &device, &status);
	double* res = (double*)d3->getHostData();
	*/
}


void test1() {

	
}
