#include <memory>
#include <iostream>
#include <string>
#include <vector>
#include <cstdio>

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
  "/usr/include/x86_64-linux-gnu/c++/4.9",
  "/usr/include/c++/4.9",
  "/usr/include/x86_64-linux-gnu",
  "/usr/include",
  0
};

class cppbrowser::Parser_impl {
public:
  vector<string> files;
  vector<string> incpath;

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
      hso.AddPath(*pdir, clang::frontend::System, false, false);
  }

  void parse(const char *filename);
};

cppbrowser::Parser::Parser():
  impl(new Parser_impl)
{}

cppbrowser::Parser::~Parser()
{}

void
cppbrowser::Parser::add_file(string &&f)
{
  impl->files.push_back(f);
}

void
cppbrowser::Parser::add_incdir(string &&dir)
{
  impl->ci.getHeaderSearchOpts().AddPath(
    dir, clang::frontend::Angled, false, false);
}

int
cppbrowser::Parser::parse_all()
{
  for (auto f = impl->files.begin(); f != impl->files.end(); ++f) {
    FILE *foo = 0;
    if (!sn_register_filename(&foo, const_cast<char *>(f->c_str())))
      fclose(foo);
    impl->parse(f->c_str());
  }
  int nerr = impl->buf.err_end() - impl->buf.err_begin();
  impl->ci.createDiagnostics();
  impl->buf.FlushDiagnostics(impl->ci.getDiagnostics());
  return nerr;
}

static inline char *
unsafe_cstr(const string &str)
{
  return const_cast<char *>(str.c_str());
}


namespace {
  class Sn_ast_visitor:
  public RecursiveASTVisitor<Sn_ast_visitor>
  {
  public:
    Sn_ast_visitor(const Parser_impl &ctx): ctx(ctx) {}

    //TODO 

    bool VisitFunctionDecl(FunctionDecl *);

  private:
    const Parser_impl &ctx;
  };

  bool
  Sn_ast_visitor::VisitFunctionDecl(FunctionDecl *f)
  {
    DeclarationNameInfo ni = f->getNameInfo();
    const SourceManager &sm = ctx.ci.getSourceManager();
    if (!sm.isInMainFile(ni.getLoc()))
      return true;
    CXXMethodDecl *meth = dynamic_cast<CXXMethodDecl *>(f);
    bool def = f->isThisDeclarationADefinition();
    int type;
    string cls;
    //TODO
    unsigned attr = 0;
    string argtypes, argnames, rettype;
    if (meth) {
      type =  def ? SN_MBR_FUNC_DEF : SN_MBR_FUNC_DCL;
      cls = meth->getParent()->getNameAsString();
    } else
      type = def ? SN_FUNC_DEF : SN_FUNC_DCL;
    string id = ni.getAsString();
    string fname = sm.getFilename(ni.getLoc());
    SourceLocation
      begin = ni.getSourceRange().getBegin(),
      end = ni.getSourceRange().getEnd();
    unsigned
      begin_line = sm.getExpansionLineNumber(begin),
      begin_col = sm.getExpansionColumnNumber(begin),
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end);
    sn_insert_symbol(
      type, (meth ? unsafe_cstr(cls) : 0), unsafe_cstr(id), 
      unsafe_cstr(fname), begin_line, begin_col, end_line, end_col, attr,
      unsafe_cstr(rettype), unsafe_cstr(argtypes), unsafe_cstr(argnames),
      0, begin_line, begin_col, end_line, end_col);
    return true;
  }


  class Sn_ast_consumer:
  public ASTConsumer
  {
  public:
    Sn_ast_consumer(const Parser_impl &ctx): vis(ctx) {}

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


void
Parser_impl::parse(const char *filename)
{
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
  ci.setASTConsumer(new Sn_ast_consumer(*this));
  ci.getDiagnosticClient().BeginSourceFile(ci.getLangOpts(), &pp);
  ParseAST(pp, &ci.getASTConsumer(), ci.getASTContext());
}
