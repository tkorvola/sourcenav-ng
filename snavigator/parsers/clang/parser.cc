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
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/PPCallbacks.h>
#include <clang/Lex/MacroInfo.h>

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

  typedef unordered_map<string, unsigned> fname_map_type;
  fname_map_type fname_map;

  TextDiagnosticBuffer buf;

  const string *orig_fname(string &&absname) const {
    auto it = fname_map.find(absname);
    return it != fname_map.end() ? &files[it->second] : 0;
  }

  /**
   * Return the original file name of loc.
   * Return null if loc is not in the main file.
   */
  const string *get_filename(const SourceManager &sm,
			     SourceLocation loc) const {
    if (!sm.isInMainFile(loc))
      return 0;
    else
      return orig_fname(sm.getFilename(loc));
  }

  /**
   * Like get_filename(const SourceManager &, SourceLocation) but checks 
   * that both ends are in the main file.
   */
  const string *get_filename(const SourceManager &sm,
			     SourceRange rng) const {
    return (sm.isInMainFile(rng.getEnd())
	    ? get_filename(sm, rng.getBegin()) : 0);
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
  impl->args.push_back("-std=c++11");
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
      impl(impl), ci(ci), pp(ci.getLangOpts())
    {}

    //TODO 

    bool VisitNamespaceDecl(NamespaceDecl *);

    bool VisitRecordDecl(RecordDecl *);

    bool VisitCXXRecordDecl(CXXRecordDecl *);

    bool VisitFieldDecl(FieldDecl *);

    bool VisitFunctionDecl(FunctionDecl *);

    bool VisitVarDecl(VarDecl *);

    const string *get_filename(SourceLocation loc) const {
      return impl.get_filename(ci.getSourceManager(), loc);
    }

    const string *get_filename(SourceRange rng) const {
      return impl.get_filename(ci.getSourceManager(), rng);
    }

  private:
    bool do_named_decl(int sntype, NamedDecl *, bool full_range,
		       const NamedDecl *cls = 0, unsigned attr = 0);

    string simple_typename(const QualType &qt);

    const Parser_impl &impl;
    const CompilerInstance &ci;
    const PrintingPolicy pp;
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
	begin_col = sm.getExpansionColumnNumber(rng.getBegin()) - 1,
	end_line = sm.getExpansionLineNumber(rng.getEnd()),
	end_col = sm.getExpansionColumnNumber(rng.getEnd()) - 1;
      sn_insert_symbol(
	SN_CLASS_INHERIT, unsafe_cstr(cname),
	unsafe_cstr(simple_typename(base->getType())), unsafe_cstr(*fname),
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
    string rettype = f->getReturnType().getAsString(pp);
    string argtypes, argnames;
    for (auto it = f->parameters().begin(); it != f->parameters().end(); ++it) {
      const ParmVarDecl &par = **it;
      argtypes += par.getType().getAsString(pp);
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
      begin_col = sm.getExpansionColumnNumber(begin) - 1,
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end) - 1;
    const char *comment = doc_comment(f);
    if (meth) {
      type =  def ? SN_MBR_FUNC_DEF : SN_MBR_FUNC_DCL;
      cls = meth->getParent()->getNameAsString();
      attr |= access_attr(f->getAccess());
      if (meth->isStatic())
	attr |= SN_STATIC;
      if (meth->isVirtual())
	attr |= meth->isPure() ? SN_PUREVIRTUAL : SN_VIRTUAL;
      if (def && meth->getLexicalDeclContext()->isRecord()) {
	// Sourcenav won't register the member unless it has a DCL.
	// So do both.
	sn_insert_symbol(
	  SN_MBR_FUNC_DCL, unsafe_cstr(cls), unsafe_cstr(id),
	  unsafe_cstr(*fname), begin_line, begin_col, end_line, end_col, attr,
	  unsafe_cstr(rettype), unsafe_cstr(argtypes), unsafe_cstr(argnames),
	  const_cast<char *>(comment),
	  begin_line, begin_col, end_line, end_col);
	// No need to repeat this.
	comment = 0;
      }
    } else {
      type = def ? SN_FUNC_DEF : SN_FUNC_DCL;
      if (f->getStorageClass() == SC_Static)
	// Gotta love the static keyword!
	attr |= SN_STATIC;
    }
    sn_insert_symbol(
      type, (meth ? unsafe_cstr(cls) : 0), unsafe_cstr(id),
      unsafe_cstr(*fname), begin_line, begin_col, end_line, end_col, attr,
      unsafe_cstr(rettype), unsafe_cstr(argtypes), unsafe_cstr(argnames),
      const_cast<char *>(comment),
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
      begin_col = sm.getExpansionColumnNumber(begin) - 1,
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end) - 1;
    sn_insert_symbol(
      sntype, cls ? unsafe_cstr(cls->getName()) : 0, 
      unsafe_cstr(decl->getName()), unsafe_cstr(*fname),
      begin_line, begin_col, end_line, end_col, attr,
      0, 0, 0, const_cast<char *>(doc_comment(decl)),
      begin_line, begin_col, end_line, end_col);
    return true;
  }

  string
  Sn_ast_visitor::simple_typename(const QualType &qt)
  {
    if (auto *ts = qt->getAs<TemplateSpecializationType>()) {
      string str;
      llvm::raw_string_ostream ostr(str);
      ts->getTemplateName().print(ostr, pp);
      return ostr.str();
    } else
      return qt.getAsString(pp);
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
    bool HandleTopLevelDecl(DeclGroupRef group) override {
      for (DeclGroupRef::iterator d = group.begin();
	   d != group.end(); ++d)
	vis.TraverseDecl(*d);
      return true;
    }
#else
    // Process after the whole translation unit has been read.
    void HandleTranslationUnit(ASTContext &ctx) override {
      vis.TraverseDecl(ctx.getTranslationUnitDecl());
    }
#endif

  private:
    Sn_ast_visitor vis;
  };

  class Sn_pp_callbacks:
  public PPCallbacks
  {
  public:
    Sn_pp_callbacks(const Parser_impl &impl, const CompilerInstance &ci):
      impl(impl), sm(ci.getSourceManager())
    {}

    void InclusionDirective(
      SourceLocation loc, const Token &inctok, StringRef incfname, bool angled,
      CharSourceRange fnrange, const FileEntry *file, StringRef spath,
      StringRef relpath, const Module *imported)
      override
    {
      if (angled)
	return;
      const string *fname = impl.get_filename(sm, loc);
      if (fname) {
	SourceLocation
	  begin = fnrange.getBegin(),
	  end = fnrange.getEnd();
	unsigned
	  begin_line = sm.getSpellingLineNumber(begin),
	  begin_col = sm.getSpellingColumnNumber(begin) - 1,
	  end_line = sm.getSpellingLineNumber(end),
	  end_col = sm.getSpellingColumnNumber(end) - 1;
	sn_insert_symbol(
	  SN_INCLUDE_DEF, 0, unsafe_cstr(incfname), unsafe_cstr(*fname),
	  begin_line, begin_col, end_line, end_col, 0, 0, 0, 0, 0,
	  begin_line, begin_col, end_line, end_col);
      }
    }

    void MacroDefined(const Token &mactok, const MacroDirective *md) override {
      if (!md || md->getKind() != MacroDirective::MD_Define)
	return;
      const MacroInfo *mi = md->getMacroInfo();
      SourceLocation 
	begin = mi->getDefinitionLoc(),
	end = mi->getDefinitionEndLoc();
      const string *fname = impl.get_filename(sm, SourceRange(begin, end));
      if (!fname || fname->empty())
	return;
      string argnames;
      for (auto id = mi->arg_begin(); id != mi->arg_end(); ++id) {
	argnames += (*id)->getName();
	argnames += ",";
      }
      if (!argnames.empty())
	argnames.pop_back();
      unsigned
	begin_line = sm.getSpellingLineNumber(begin),
	begin_col = sm.getSpellingColumnNumber(begin) - 1,
	end_line = sm.getSpellingLineNumber(end),
	end_col = sm.getSpellingColumnNumber(end) - 1;
      sn_insert_symbol(
	SN_MACRO_DEF, 0, unsafe_cstr(mactok.getIdentifierInfo()->getName()),
	unsafe_cstr(*fname), begin_line, begin_col, end_line, end_col,
	0, 0, 0, unsafe_cstr(argnames), 0, begin_line, begin_col, 
	end_line, end_col);
    }

    void MacroExpands(
      const Token &mactok, const MacroDirective *def, SourceRange range,
      const MacroArgs *args) 
      override
    {
      const string *fname = impl.get_filename(sm, range);
      if (!fname)
	return;
      /* sn_insert_xref takes as parameter the function where the
	 reference occurred.  The preprocessor cannot know that!
	 So we just put in SN_SUBR_DEF and nulls. */
      SourceLocation loc = range.getBegin();
      unsigned line = sm.getSpellingLineNumber(loc);
      sn_insert_xref(
	SN_REF_TO_DEFINE, SN_SUBR_DEF, SN_REF_SCOPE_GLOBAL, 0, 0, 0, 0,
	unsafe_cstr(mactok.getIdentifierInfo()->getName()), 0,
	unsafe_cstr(*fname), line, SN_REF_READ);
    }

  private:
    const Parser_impl &impl;
    const SourceManager &sm;
  };

  class Sn_action:
  public ASTFrontendAction
  {
  public:
    Sn_action(const Parser_impl &impl): impl(impl) {}

    bool BeginSourceFileAction(CompilerInstance &ci, StringRef f) override {
      FILE *foo = 0;
      const string *fname = impl.orig_fname(f);
      if (!fname) {
	cerr << "Skipping \"" << f.str() << '"' << endl;
	return false;
      }
      if (!sn_register_filename(&foo, unsafe_cstr(*fname)))
	fclose(foo);
      ci.getPreprocessor().addPPCallbacks(new Sn_pp_callbacks(impl, ci));
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
