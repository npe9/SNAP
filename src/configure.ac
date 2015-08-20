#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIRS([m4])
AX_OPENMP
# Checks for programs.
AC_PROG_CC
AC_PROG_FC

FCFLAGS=" -g -Wall -fno-backtrace $FCFLAGS"
# Checks for libraries.
AC_CHECK_LIB([gomp], [omp_get_num_threads]) 
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([rt], [aio_read])
AC_CHECK_LIB([m], [round])
AC_CHECK_LIB([mpi], [mpi_init])
AC_ARG_WITH([xpmem],
			[AS_HELP_STRING([--with-xpmem],
							[path to nonstandard location for xpmem library])],
							[
							LDFLAGS="-L$withval/lib $LDFLAGS"
							CFLAGS="-I$withval/include $CFLAGS"
							],
							[])
AC_CHECK_LIB([xpmem], [xpmem_get], [], [AC_MSG_ERROR([xpmem required but not found])])

AC_ARG_WITH([hobbes],
			[AS_HELP_STRING([--with-hobbes],
							[path to nonstandard location for the hobbes library])],
							[
							LDFLAGS="-L$withval $LDFLAGS"
							CFLAGS="-I$withval $CFLAGS"
							],
							[])
AC_CHECK_LIB([hobbes], [xemem_get], [], [AC_MSG_ERROR([hobbes required but not found])])

AC_ARG_WITH([kitten],
			[AS_HELP_STRING([--with-kitten],
							[path to nonstandard location for kitten and its lwk library])],
							[
							LDFLAGS="-L$withval/user/liblwk $LDFLAGS"
							CFLAGS="-I$withval/include -I$withval/user/liblwk $CFLAGS"
							],
							[])
AC_CHECK_LIB([lwk], [aspace_map_region], [], [AC_MSG_ERROR([liblwk required but not found])])


# Checks for header files.
AC_CHECK_HEADERS([fcntl.h unistd.h xpmem.h xemem.h]) #, [], [AC_MSG_ERROR([required header not found]])
AC_CHECK_HEADERS([sys/time.h])
# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_TYPE_UINT64_T

# Checks for library functions.
AC_FUNC_MMAP
AC_CHECK_FUNCS([ftruncate gettimeofday munmap])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
