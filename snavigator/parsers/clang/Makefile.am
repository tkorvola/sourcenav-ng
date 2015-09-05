AUTOMAKE_OPTIONS = foreign

toolbox = $(top_builddir)/parsers/toolbox
common = $(top_builddir)/parsers/common

libexec_PROGRAMS = cppbrowser

cppbrowser_SOURCES = main.cc parser.cc
cppbrowser_CXXFLAGS = $(LLVM_CXXFLAGS)
cppbrowser_LDFLAGS = $(LLVM_LDFLAGS)
cppbrowser_LIBS = $(LLVM_LIBS)
