#include <memory>
#include <iostream>
#include <string>
#include <vector>
#include <cstdio>
#include <unordered_map>

#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Tooling/CompilationDatabase.h>
#include <clang/Tooling/Tooling.h>

extern "C" {
#include "snptools.h"
}

#include "parser.hh"

using namespace std;
using namespace clang;
using namespace clang::tooling;

using namespace cppbrowser;

class cppbrowser::Parser_impl {
public:
  vector<string> files;
  vector<string> args;

  unordered_map<string, unsigned> fname_map;

  TextDiagnosticBuffer buf;

  const string &orig_fname(string &&absname) const {
    return files[fname_map.at(absname)];
  }

  void add_file(string &&f) {
    unsigned i = files.size();
    files.push_back(f);
    fname_map[f] = fname_map[getAbsolutePath(f)] = i;
  }
};

cppbrowser::Parser::Parser():
  impl(new Parser_impl)
{}

cppbrowser::Parser::~Parser()
{}

void
cppbrowser::Parser::add_file(string &&f)
{
  impl->add_file(move(f));
}

void
cppbrowser::Parser::add_incdir(string &&dir)
{
  impl->args.push_back("-I" + dir);
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
    Sn_ast_visitor(const Parser_impl &impl, CompilerInstance &ci):
      impl(impl), ci(ci)
    {}

    //TODO 

    bool VisitFunctionDecl(FunctionDecl *);

  private:
    const Parser_impl &impl;
    const CompilerInstance &ci;
  };

  bool
  Sn_ast_visitor::VisitFunctionDecl(FunctionDecl *f)
  {
    DeclarationNameInfo ni = f->getNameInfo();
    const SourceManager &sm = ci.getSourceManager();
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
    const string &fname = impl.orig_fname(sm.getFilename(ni.getLoc()));
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
    Sn_ast_consumer(const Parser_impl &impl, CompilerInstance &ci):
      vis(impl, ci)
    {}

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

  class Sn_action:
  public ASTFrontendAction
  {
  public:
    Sn_action(const Parser_impl &impl): impl(impl) {}

    bool BeginSourceFileAction(CompilerInstance &ci, StringRef f) override {
      FILE *foo = 0;
      if (!sn_register_filename(
	    &foo, const_cast<char *>(impl.orig_fname(f).c_str())))
	fclose(foo);
      return true;
    }

    ASTConsumer *
    CreateASTConsumer(CompilerInstance &ci, StringRef file) override {
      return new Sn_ast_consumer(impl, ci);
    }

  private:
    const Parser_impl &impl;
  };

  class Sn_factory:
  public FrontendActionFactory
  {
  public:
    Sn_factory(const Parser_impl &impl): impl(impl) {}

    FrontendAction *create() override {return new Sn_action(impl);}

  private:
    const Parser_impl &impl;
  };
}

int
cppbrowser::Parser::parse_all()
{
  FixedCompilationDatabase cdb(".", impl->args);
  ClangTool tool(cdb, impl->files);
  DiagnosticOptions dopt;
  Sn_factory fac(*impl);
  tool.setDiagnosticConsumer(&impl->buf);
  tool.run(&fac);
  int nerr = impl->buf.err_end() - impl->buf.err_begin();
  return nerr;
}
