#include <memory>
#include <iostream>
#include <string>

#include <llvm/Support/Host.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
#include <clang/Lex/HeaderSearch.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Parse/ParseAST.h>

extern "C" {
#include "snptools.h"
}

#include "parser.hh"

using namespace std;
using namespace clang;

using namespace cppbrowser;

static const char *builtin_includes[] = {
  "/usr/local/include",
  "/usr/include/c++/4.9",
  "/usr/include",
  0
};

class cppbrowser::Parser_impl
{
public:
  CompilerInstance ci;
  TextDiagnosticBuffer buf;

  Parser_impl() {
    ci.createDiagnostics(&buf, false);
    ci.createFileManager();
    shared_ptr<TargetOptions> to = make_shared<TargetOptions>();
    to->Triple = llvm::sys::getDefaultTargetTriple();
    ci.setTarget(TargetInfo::CreateTargetInfo(ci.getDiagnostics(), to));

    HeaderSearchOptions &hso = ci.getHeaderSearchOpts();
    hso.UseStandardSystemIncludes = hso.UseStandardCXXIncludes
      = hso.UseBuiltinIncludes = hso.Verbose = 1;
    for (const char **pdir = builtin_includes; *pdir; ++pdir)
      hso.AddPath(*pdir, clang::frontend::Angled, false, false);
    for (const char *dir = sn_includepath_first();
	 dir; dir = sn_includepath_next())
      hso.AddPath(dir, clang::frontend::Angled, false, false);
  }
};

cppbrowser::Parser::Parser()
{}

cppbrowser::Parser::~Parser()
{}

namespace {
  class Sn_ast_visitor:
  public RecursiveASTVisitor<Sn_ast_visitor>
  {
  public:
    Sn_ast_visitor(const ASTContext &ctr): ctx(ctx) {}

    //TODO

    bool
    VisitFunctionDecl(FunctionDecl *f)
    {
      CXXMethodDecl *meth = dynamic_cast<CXXMethodDecl *>(f);
      int type = (f->isThisDeclarationADefinition()
		  ? (meth ? SN_MBR_FUNC_DEF : SN_FUNC_DEF)
		  : (meth ? SN_MBR_FUNC_DCL : SN_FUNC_DCL));
      string id = f->getNameInfo().getAsString();
      cout << (meth ? "mf" : "fun") << type << id << endl;
      return true;
    }

  private:
    const ASTContext &ctx;
  };

  class Sn_ast_consumer:
  public ASTConsumer
  {
  public:
    Sn_ast_consumer(const ASTContext &ctx): vis(ctx) {}

#if 1
    // Process as we read.
    bool
    HandleTopLevelDecl(DeclGroupRef group) override
    {
      for (DeclGroupRef::iterator d = group.begin();
	   d != group.end(); ++d)
	vis.TraverseDecl(*d);
      return true;
    }
#else
    // Process after the whole translation unit has been read.
    void
    HandleTranslationUnit(ASTContext &ctx) override
    {
      vis.TraverseDecl(ctx.getTranslationUnitDecl());
    }
#endif

  private:
    Sn_ast_visitor vis;
  };
}

int
cppbrowser::Parser::parse(const char *filename)
{
  if (!impl)
    impl.reset(new Parser_impl);
  CompilerInstance &ci = impl->ci;
  ci.createSourceManager(ci.getFileManager());
  SourceManager &sm = ci.getSourceManager();
  sm.setMainFileID(sm.createFileID(ci.getFileManager().getFile(filename),
				   SourceLocation(), SrcMgr::C_User));
  ci.getLangOpts().CPlusPlus = 1;
  // Or maybe TU_Prefix for headers?
  ci.createPreprocessor(clang::TU_Complete);
  Preprocessor &pp = ci.getPreprocessor();
  ci.getPreprocessorOpts().UsePredefines = true;
  ci.createASTContext();
  ci.setASTConsumer(new Sn_ast_consumer(ci.getASTContext()));
  ci.getDiagnosticClient().BeginSourceFile(ci.getLangOpts(), &pp);
  ParseAST(pp, &ci.getASTConsumer(), ci.getASTContext());
  ci.createDiagnostics();
  impl->buf.FlushDiagnostics(ci.getDiagnostics());
}

void
cppbrowser::Parser::reset()
{
  //TODO
}
