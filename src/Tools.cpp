#include"Tools.h"
using namespace std;
#include <iostream>
#include "R.h"

void errorHandle(std::string errorInfo) {
#ifdef VS
	std::cout << errorInfo.c_str() << std::endl;
#else
	error(errorInfo.c_str());
	Rprintf("\n");
#endif // VS
}

void message(std::string msg) {
#ifdef VS
	std::cout << msg.c_str()<<std::endl;
#else
	Rprintf(msg.c_str());
	Rprintf("\n");
#endif // VS
}