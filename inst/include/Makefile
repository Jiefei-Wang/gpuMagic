.PHONY: clean ICD

CPPFLAGS  = -I./inc -DCL_TARGET_OPENCL_VERSION=220
ODIR=lib$(WIN)


OBJ=$(ODIR)/icd.o $(ODIR)/icd_dispatch.o $(ODIR)/$(ICD_OS).o

openCL_OBJ_FULL_PATH=$(ODIR)/$(openCL_OBJ)

ICD: $(openCL_OBJ_FULL_PATH)


$(openCL_OBJ_FULL_PATH): $(OBJ)
	@mkdir -p $(ODIR)
	$(AR) rcs $@ $^
	$(MAKE) clean

	
$(ODIR)/%.o:%.c
	@mkdir -p $(ODIR)
	$(CC) -o $@ -c $^ $(CFLAGS) $(CPPFLAGS)

clean:
	$(RM) $(ODIR)/*.o