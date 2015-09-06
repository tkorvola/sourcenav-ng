AUTOMAKE_OPTIONS = foreign

toolbox = $(top_builddir)/../toolbox
common = $(top_builddir)/../common
libutils = $(top_builddir)/../../misc/libutils

libexec_PROGRAMS = cppbrowser

AM_CPPFLAGS = $(WINFLAGS) \
	-I$(top_srcdir)/../../hyper \
	-I$(top_srcdir)/../toolbox \
	-I$(top_srcdir)/../common \
	-I$(top_srcdir)/../..

CLANG_LIBS = -lclangTooling -lclangFrontendTool -lclangFrontend \
	-lclangDriver -lclangSerialization -lclangCodeGen -lclangParse \
	-lclangSema -lclangStaticAnalyzerFrontend \
	-lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore \
	-lclangAnalysis -lclangARCMigrate -lclangRewrite \
	-lclangRewriteFrontend -lclangEdit -lclangAST -lclangLex -lclangBasic

cppbrowser_SOURCES = main.cc parser.cc
cppbrowser_CXXFLAGS = $(LLVM_CXXFLAGS)
cppbrowser_LDFLAGS = $(LLVM_LDFLAGS)
cppbrowser_LDADD = $(toolbox)/libsnptools.a \
		$(common)/libcommon.a \
		$(libutils)/libutils.a \
		$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS) \
		$(CLANG_LIBS) $(LLVM_LIBS) -lz -lpthread -ltinfo
