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

cppbrowser::Parser::Parser(bool all_cxx):
  impl(new Parser_impl)
{
  //impl->args.push_back("-std=c++11");
  if (all_cxx)
    impl->args.push_back("-xc++");
}

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


static const char *
doc_comment(Decl *decl)
{
  const ASTContext &ctx = decl->getASTContext();
  RawComment *doc = ctx.getRawCommentForDeclNoCache(decl);
  return doc && doc->isDocumentation() ? doc->getBriefText(ctx) : 0;
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

    bool VisitNamespaceDecl(NamespaceDecl *);

    bool VisitRecordDecl(RecordDecl *);

    bool VisitCXXRecordDecl(CXXRecordDecl *);

    bool VisitFieldDecl(FieldDecl *);

    bool VisitFunctionDecl(FunctionDecl *);

    bool VisitVarDecl(VarDecl *);

    /**
     * Return the original file name of loc.
     * Return null if loc is not in the main file.
     * The returned string is owned by Parser_impl.
     */
    const string *get_filename(SourceLocation loc) const {
      const SourceManager &sm = ci.getSourceManager();
      if (!sm.isInMainFile(loc))
	return 0;
      else
	return &impl.orig_fname(sm.getFilename(loc));
    }

    /**
     * Like get_filename(SourceLocation) but checks that both ends are
     * in the main file.
     */
    const string *get_filename(SourceRange rng) const {
      return (ci.getSourceManager().isInMainFile(rng.getEnd())
	      ? get_filename(rng.getBegin()) : 0);
    }

  private:
    bool do_named_decl(int sntype, NamedDecl *, bool full_range,
		       const NamedDecl *cls = 0, unsigned attr = 0);

    const Parser_impl &impl;
    const CompilerInstance &ci;
  };

  static unsigned 
  access_attr(AccessSpecifier access)
  {
    switch (access) {
      case AS_public:
	return SN_PUBLIC;
      case AS_protected:
	return SN_PROTECTED;
      case AS_private:
	return SN_PRIVATE;
    } 
    return 0;
  }

  bool
  Sn_ast_visitor::VisitNamespaceDecl(NamespaceDecl *ns)
  {
    // Don't know what to do with these.
    if (ns->isAnonymousNamespace())
      return true;
    return do_named_decl(SN_NAMESPACE_DEF, ns, false);
  }

  bool
  Sn_ast_visitor::VisitRecordDecl(RecordDecl *st)
  {
    if (st->isCompleteDefinition())
      return do_named_decl(SN_CLASS_DEF, st, false);
    else
      return true;
  }

  bool
  Sn_ast_visitor::VisitCXXRecordDecl(CXXRecordDecl *cls)
  {
    StringRef cname = cls->getName();
    if (!cls->isCompleteDefinition())
      return true;
    const SourceManager &sm = ci.getSourceManager();
    for (auto base = cls->bases_begin(); base != cls->bases_end(); ++base) {
      SourceRange rng = base->getSourceRange();
      const string *fname = get_filename(rng);
      if (!fname)
	continue;
      unsigned attr = access_attr(base->getAccessSpecifier());
      if (base->isVirtual())
	attr |= SN_VIRTUAL;
      unsigned
	begin_line = sm.getExpansionLineNumber(rng.getBegin()),
	begin_col = sm.getExpansionColumnNumber(rng.getBegin()),
	end_line = sm.getExpansionLineNumber(rng.getEnd()),
	end_col = sm.getExpansionColumnNumber(rng.getEnd());
      sn_insert_symbol(
	SN_CLASS_INHERIT, unsafe_cstr(cname),
	unsafe_cstr(base->getType().getAsString()), unsafe_cstr(*fname),
	begin_line, begin_col, end_line, end_col, attr, 0, 0, 0, 0,
	begin_line, begin_col, end_line, end_col);
    }
    return true;
  }

  bool
  Sn_ast_visitor::VisitFieldDecl(FieldDecl *f)
  {
    if (f->isAnonymousStructOrUnion())
      return true;
    do_named_decl(SN_MBR_VAR_DEF, f, true, f->getParent());
    //TODO: xref type
  }


  bool
  Sn_ast_visitor::VisitFunctionDecl(FunctionDecl *f)
  {
    DeclarationNameInfo ni = f->getNameInfo();
    const string *fname = get_filename(ni.getLoc());
    if (!fname)
      return true;
    const SourceManager &sm = ci.getSourceManager();
    CXXMethodDecl *meth = dynamic_cast<CXXMethodDecl *>(f);
    bool def = f->isThisDeclarationADefinition();
    int type;
    string cls;
    unsigned attr = 0;
    string rettype = f->getReturnType().getAsString();
    string argtypes, argnames;
    if (meth) {
      type =  def ? SN_MBR_FUNC_DEF : SN_MBR_FUNC_DCL;
      cls = meth->getParent()->getNameAsString();
      attr |= access_attr(f->getAccess());
      if (meth->isStatic())
	attr |= SN_STATIC;
      if (meth->isVirtual())
	attr |= SN_VIRTUAL;
    } else {
      type = def ? SN_FUNC_DEF : SN_FUNC_DCL;
      if (f->getStorageClass() == SC_Static)
	// Gotta love the static keyword!
	attr |= SN_STATIC;
    }
    for (auto it = f->parameters().begin(); it != f->parameters().end(); ++it) {
      const ParmVarDecl &par = **it;
      argtypes += par.getType().getAsString();
      argtypes += ",";
      argnames += par.getName();
      argnames += ",";
    }
    if (!argtypes.empty())
      argtypes.pop_back();
    if (!argnames.empty())
      argnames.pop_back();

    string id = ni.getAsString();
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
      unsafe_cstr(*fname), begin_line, begin_col, end_line, end_col, attr,
      unsafe_cstr(rettype), unsafe_cstr(argtypes), unsafe_cstr(argnames),
      const_cast<char *>(doc_comment(f)),
      begin_line, begin_col, end_line, end_col);
    return true;
  }

  bool
  Sn_ast_visitor::VisitVarDecl(VarDecl *var)
  {
    int type;
    unsigned attr;
    NamedDecl *cls = 0;
    DeclContext *ctx = var->getDeclContext();
    bool def = (var->isThisDeclarationADefinition() == VarDecl::Definition);
    if (var->isStaticDataMember()) {
      type = SN_MBR_VAR_DEF;
      attr |= SN_STATIC;
      cls = static_cast<TagDecl *>(ctx);
    } else if (var->hasGlobalStorage()) {
      type = def ? SN_GLOB_VAR_DEF : SN_VAR_DCL;
      if (var->isFileVarDecl())
	attr |= SN_STATIC;
    } else if (var->isLocalVarDecl()) {
      type = SN_LOCAL_VAR_DEF;
      if (var->isStaticLocal())
	attr |= SN_STATIC;
      //TODO: xref type
    } else
      return true;
    return do_named_decl(type, var, true, cls, attr);
  }


  bool
  Sn_ast_visitor::do_named_decl(
    int sntype, NamedDecl *decl, bool full_range,
    const NamedDecl *cls, unsigned attr)
  {
    SourceLocation loc = decl->getLocStart();
    const string *fname = get_filename(loc);
    if (!fname)
      return true;
    attr |= access_attr(decl->getAccess());
    const SourceManager &sm = ci.getSourceManager();
    // Doesn't look like we can find the exact location of the name.
    SourceLocation
      begin = decl->getLocStart(),
      end = full_range ? decl->getLocEnd() : decl->getLocStart();
    unsigned
      begin_line = sm.getExpansionLineNumber(begin),
      begin_col = sm.getExpansionColumnNumber(begin),
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end);
    unsigned
      line = sm.getExpansionLineNumber(loc),
      col = sm.getExpansionColumnNumber(loc);
    sn_insert_symbol(
      sntype, cls ? unsafe_cstr(cls->getName()) : 0, 
      unsafe_cstr(decl->getName()), unsafe_cstr(*fname),
      begin_line, begin_col, end_line, end_col, attr,
      0, 0, 0, const_cast<char *>(doc_comment(decl)),
      begin_line, begin_col, end_line, end_col);
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
