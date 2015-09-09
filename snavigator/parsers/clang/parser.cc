#include <memory>

#include <llvm/Support/Host.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
#include <clang/Lex/HeaderSearch.h>

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
    ci.createDiagnostics(&buf, false);
    ci.createFileManager();
    ci.createSourceManager(ci.getFileManager());
    std::shared_ptr<TargetOptions> to = std::make_shared<TargetOptions>();
    to->Triple = llvm::sys::getDefaultTargetTriple();
    ci.setTarget(TargetInfo::CreateTargetInfo(ci.getDiagnostics(), to));

    HeaderSearchOptions &hso = ci.getHeaderSearchOpts();
    for (const char *dir = sn_includepath_first();
	 dir; dir = sn_includepath_next())
      hso.AddPath(dir, clang::frontend::Angled, false, false);
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
  CompilerInstance &ci = impl->ci;
  // Or maybe TU_Prefix for headers?
  ci.createPreprocessor(clang::TU_Complete);
  ci.getPreprocessorOpts().UsePredefines = true;
}

void
cppbrowser::Parser::reset()
{
  //TODO
}
