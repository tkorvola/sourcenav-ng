#include <llvm/ADT/IntrusiveRefCntPtr.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>

extern "C" {
#include "snptools.h"
}

#include "parser.hh"

using namespace clang;

using namespace cppbrowser;

class cppbrowser::Parser_impl
{
public:
  CompilerInstance ci;
  TextDiagnosticBuffer buf;

  Parser_impl() {
    ci.createDiagnostics(&buf);
    ci.createFileManager();
    ci.createSourceManager(ci.getFileManager());
  }
};

cppbrowser::Parser::Parser():
  impl(new Parser_impl)
{}

cppbrowser::Parser::~Parser()
{}

int
cppbrowser::Parser::parse(const char *filename)
{
  //TODO
}

void
cppbrowser::Parser::reset()
{
  //TODO
}
