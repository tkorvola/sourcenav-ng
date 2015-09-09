#include <memory>

#include <llvm/Support/Host.h>
#include <llvm/ADT/IntrusiveRefCntPtr.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
// #include <clang/Lex/HeaderSearch.h>

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
  std::shared_ptr<TargetOptions> to;
  llvm::IntrusiveRefCntPtr<HeaderSearchOptions> hso;
  //std::unique_ptr<HeaderSearch> hs;

  Parser_impl():
    to(std::make_shared<TargetOptions>()),
    hso(new HeaderSearchOptions)
  {
    ci.createDiagnostics(&buf, false);
    ci.createFileManager();
    ci.createSourceManager(ci.getFileManager());
    to->Triple = llvm::sys::getDefaultTargetTriple();
    ci.setTarget(TargetInfo::CreateTargetInfo(ci.getDiagnostics(), to));
    // hs.reset(
    //   new HeaderSearch(
    // 	hso, ci.getSourceManager(), ci.getDiagnostics(), ci.getLangOpts(),
    // 	TargetInfo::CreateTargetInfo(ci.getDiagnostics(), ci.getTargetOpts())));
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
  // Or maybe TU_Prefix for headers?
  impl->ci.createPreprocessor(clang::TU_Complete);
  impl->ci.getPreprocessorOpts().UsePredefines = true;
}

void
cppbrowser::Parser::reset()
{
  //TODO
}
