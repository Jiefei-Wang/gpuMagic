.PHONY: ICD_WIN ICD_LINUX

CPPFLAGS  = -I./inc -DCL_TARGET_OPENCL_VERSION=220
ODIR=output
CLDIR=lib


OBJ=$(ODIR)/icd.o $(ODIR)/icd_dispatch.o $(ODIR)/$(ICD_OS).o


ICD_WIN: $(CLDIR)/OpenCL.lib
ICD_LINUX: $(CLDIR)/OpenCL.so


$(CLDIR)/OpenCL.lib: $(OBJ)
	@mkdir -p $(CLDIR)
	$(AR) rcs $@ $^
	

	
$(ODIR)/%.o:%.c
	@mkdir -p $(ODIR)
	$(CC) -o $@ -c $^ $(CFLAGS) $(CPPFLAGS)


