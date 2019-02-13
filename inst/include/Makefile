.PHONY: ICD_WIN ICD_LINUX

CPPFLAGS  = -I./inc -DCL_TARGET_OPENCL_VERSION=220
ODIR=output
CLDIR=lib


OBJ=$(ODIR)/icd.o $(ODIR)/icd_dispatch.o $(ODIR)/$(ICD_OS).o


ICD_WIN: $(CLDIR)/OpenCL.lib
ICD_LINUX: $(CLDIR)/libOpenCL.a


$(CLDIR)/OpenCL.lib: $(OBJ)
	@mkdir -p $(CLDIR)
	$(AR) rcs $@ $^

$(CLDIR)/libOpenCL.a: $(OBJ)
	@mkdir -p $(CLDIR)
	$(AR) rcs $@ $^

	
$(ODIR)/%.o:%.c
	@mkdir -p $(ODIR)
	$(CC) -o $@ -c $^ $(CFLAGS) $(CPPFLAGS)


