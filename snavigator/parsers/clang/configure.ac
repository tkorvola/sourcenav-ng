dnl Process this file with autoconf to produce a configure script.

dnl Initialization.
AC_INIT(sourcenavigator,NG4.5)
AC_CONFIG_AUX_DIR(../../..)
AM_INIT_AUTOMAKE

AC_PROG_CXX

CYG_AC_PATH_TCLCONFIG
CYG_AC_LOAD_TCLCONFIG
AC_SUBST(TCL_BUILD_LIB_SPEC)
AC_SUBST(TCL_LIBS)

LLVM_CXXFLAGS="`llvm-config --cxxflags`"
LLVM_LDFLAGS="`llvm-config --ldflags`"
LLVM_LIBS="`llvm-config --libs`"
AC_SUBST([LLVM_CXXFLAGS])
AC_SUBST([LLVM_LDFLAGS])
AC_SUBST([LLVM_LIBS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
