AUTOMAKE_OPTIONS = foreign

toolbox = $(top_builddir)/../toolbox
common = $(top_builddir)/../common

libexec_PROGRAMS = cppbrowser

INCLUDES = $(WINFLAGS) \
	-I$(top_srcdir)/../../hyper \
	-I$(top_srcdir)/../toolbox \
	-I$(top_srcdir)/../common \
	-I$(top_srcdir)/../..

cppbrowser_SOURCES = main.cc parser.cc
cppbrowser_CXXFLAGS = $(LLVM_CXXFLAGS)
cppbrowser_LDFLAGS = $(LLVM_LDFLAGS)
cppbrowser_LDADD = $(LLVM_LIBS)
