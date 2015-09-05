dnl Process this file with autoconf to produce a configure script.

dnl Initialization.
AC_INIT(sourcenavigator,NG4.5)
AM_INIT_AUTOMAKE

AC_PROG_CXX

LLVM_CXXFLAGS="`llvm-config --cxxflags`"
LLVM_LDFLAGS="`llvm-config --ldflags`"
LLVM_LIBS="`llvm-config --libs`"
AC_SUBST([LLVM_CXXFLAGS])
AC_SUBST([LLVM_LDFLAGS])
AC_SUBST([LLVM_LIBS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
