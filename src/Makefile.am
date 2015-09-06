AUTOMAKE_OPTIONS = foreign
SNAP_COMMON = control.f90 data.f90 dealloc.f90 dim1_sweep.f90 dim3_sweep.f90 expxs.f90 geom.f90 global.f90 inner.f90 input.f90 mms.f90 octsweep.f90 outer.f90 output.f90 plib.f90 setup.f90 snap_main.f90 sn.f90 solvar.f90 sweep.f90 time.f90 translv.f90 utils.f90 version.f90 analytics.c gfortran_iargc.c mpidummy.c 
snap_kitten_tcasm_LDFLAGS =  -static -Wl,--whole-archive,--no-whole-archive -fopenmp # -fcheck=all #-fno-backtrace
snap_kitten_tcasm_SOURCES =  control.f90 data.f90 dealloc.f90 dim1_sweep.f90 dim3_sweep.f90 expxs.f90 geom.f90 global.f90 inner.f90 input.f90 mms.f90 octsweep.f90 outer.f90 output.f90 plib.f90 setup.f90 snap_main.f90 sn.f90 solvar.f90 sweep.f90 time.f90 translv.f90 utils.f90 version.f90 analytics.c gfortran_iargc.c mpidummy.c  syscalls-kitten-tcasm-enclave.c

snap_linux_null_LDFLAGS =  -static -Wl,--whole-archive,--no-whole-archive -fopenmp #-fcheck=all #-fno-backtrace
#snap_linux_null_SOURCES = $SNAP_COMMON syscalls-linux-null-local.c
snap_linux_null_SOURCES =  control.f90 data.f90 dealloc.f90 dim1_sweep.f90 dim3_sweep.f90 expxs.f90 geom.f90 global.f90 inner.f90 input.f90 mms.f90 octsweep.f90 outer.f90 output.f90 plib.f90 setup.f90 snap_main.f90 sn.f90 solvar.f90 sweep.f90 time.f90 translv.f90 utils.f90 version.f90 analytics.c gfortran_iargc.c mpidummy.c  syscalls-linux-null-local.c

snap_kitten_xemem_LDFLAGS =  -static -Wl,--whole-archive,--no-whole-archive -fopenmp # -fcheck=all #-fno-backtrace
snap_kitten_xemem_SOURCES =  control.f90 data.f90 dealloc.f90 dim1_sweep.f90 dim3_sweep.f90 expxs.f90 geom.f90 global.f90 inner.f90 input.f90 mms.f90 octsweep.f90 outer.f90 output.f90 plib.f90 setup.f90 snap_main.f90 sn.f90 solvar.f90 sweep.f90 time.f90 translv.f90 utils.f90 version.f90 analytics.c gfortran_iargc.c mpidummy.c  syscalls-kitten-tcasm-enclave.c

snap_linux_xpmem_LDFLAGS =  -static -Wl,--whole-archive,--no-whole-archive -fopenmp # -fcheck=all #-fno-backtrace
snap_linux_xpmem_SOURCES =  control.f90 data.f90 dealloc.f90 dim1_sweep.f90 dim3_sweep.f90 expxs.f90 geom.f90 global.f90 inner.f90 input.f90 mms.f90 octsweep.f90 outer.f90 output.f90 plib.f90 setup.f90 snap_main.f90 sn.f90 solvar.f90 sweep.f90 time.f90 translv.f90 utils.f90 version.f90 analytics.c gfortran_iargc.c mpidummy.c  syscalls-linux-xpmem-local.c


bin_PROGRAMS = snap_kitten_tcasm snap_linux_null snap_kitten_xemem snap_linux_xpmem
