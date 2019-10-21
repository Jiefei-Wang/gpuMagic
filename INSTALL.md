The package requires an ICD loader and a opencl-enabled CPU/GPU driver. This installation guide will show you how to install the dependecies step-by-step.

#Windows
For windows users, the easiest way to check if you have an ICD loader installed is to search for the file `OpenCL.dll` under `C:\Windows\System32\` directory. If the file does exist and gpuMagic package can find your devices through `getDeviceList`, you can safely skip this instruction. Otherwise, please follow the instructions below to install/reinstall graphic drivers. Usually the ICD loader will be installed by a driver, there is no need to install an ICD loader.

##Find the name of GPU(s)
1.Press win+R shortcut

2.type `CMD` and press OK button

3.type `wmic path win32_VideoController get name` in the popped up black window, then you can see the name and vendor of your GPU(s).

##Install the driver
According to your GPU vendor:

For intel:

https://downloadcenter.intel.com/product/80939/Graphics-Drivers

For Nvidia: 

https://www.nvidia.com/Download/index.aspx?lang=en-us

For AMD/ATI: 

https://www.amd.com/en/support/kb/faq/gpu-131

If vendors offer a selection between developer and gamer drivers(at leaset Nvidia does it), please choose the developer one.

##Reboot and check the ICD loader again
After the installation, reboot your system and check the file `OpenCL.dll` again, if it still does not exist, please contact the vendor through its support website to locate the `OpenCL.dll`. It may be necessary to manually move this file to fix this issue. The ICD loader can also be compiled from the source code, please see https://github.com/KhronosGroup/OpenCL-ICD-Loader for reference.



#Linux
##Install the driver
Like on windows, please go to the vendor's support site to find the correct driver and install it.

##Install Installable Client Driver(ICD)
Please run the following command to install ICD:

`sudo apt update`

`sudo apt install ocl-icd-opencl-dev`

##Check if the device is available
You can use the command-line tool `clinfo` to check the device availability.

To install `clinfo`, run

`sudo apt-get install clinfo`

If you can see your devices, the drivers are installed and you should be able to use the package. If you cannot see any device, you may need to reinstall the driver and see if it works.

#MAC
There is no special requirement for MAC, the package can be installed normally.





