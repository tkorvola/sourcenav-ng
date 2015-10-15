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
#include <clang/AST/ParentMap.h>
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

  const string *orig_fname(const string &absname) const {
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
    fname_map[f] = fname_map[getAbsolutePath(f)] = files.size();
    files.push_back(move(f));
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
  impl->args.push_back("-I" + move(dir));
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

    bool VisitTypedefNameDecl(TypedefNameDecl *);

    bool VisitDeclRefExpr(DeclRefExpr *);

    bool VisitMemberExpr(MemberExpr *);

    bool VisitDeclStmt(DeclStmt *);

    const string *get_filename(SourceLocation loc) const {
      return impl.get_filename(ci.getSourceManager(), loc);
    }

    const string *get_filename(SourceRange rng) const {
      return impl.get_filename(ci.getSourceManager(), rng);
    }

  private:
    void parse_arglist(const FunctionDecl *f,
                       string *argtypes, string *argnames = 0) const;

    bool insert_decl(
      int sntype, NamedDecl *decl, const SourceRange &loc,
      const NamedDecl *cls = 0, unsigned attr = 0, const char *rettype = 0,
      const char *argtypes = 0, const char *argnames = 0,
      bool comment = true) const;

    bool insert_decl(int sntype, NamedDecl *decl, bool full_range,
                     const NamedDecl *cls = 0, unsigned attr = 0) const {
      return insert_decl(
        sntype, decl,
        full_range ? decl->getSourceRange() : decl->getLocStart(),
        cls, attr);
    }

    int xref_type(const Type &ty, Stmt *refr, SourceLocation loc) const;

    int xref_decl(NamedDecl *tgt, Stmt *refr) const;

    int xref_decl(int snreftype, int acc, NamedDecl *tgt, Stmt *refr,
                  SourceLocation loc, const char *argtypes = 0) const;

    string simple_typename(const QualType &qt) const;

    const Parser_impl &impl;
    const CompilerInstance &ci;
    const PrintingPolicy pp;
    unique_ptr<ParentMap> pm;
    const CXXRecordDecl *pcls;
    const FunctionDecl *pfun;
    string pargtypes;
  };

  bool
  Sn_ast_visitor::VisitNamespaceDecl(NamespaceDecl *ns)
  {
    if (!ns->isAnonymousNamespace())
      insert_decl(SN_NAMESPACE_DEF, ns, false);
    return true;
  }

  bool
  Sn_ast_visitor::VisitRecordDecl(RecordDecl *st)
  {
    if (st->isCompleteDefinition())
      insert_decl(SN_CLASS_DEF, st, false);
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
    if (!f->isAnonymousStructOrUnion())
      insert_decl(SN_MBR_VAR_DEF, f, true, f->getParent());
    return true;
  }

  bool
  Sn_ast_visitor::VisitFunctionDecl(FunctionDecl *f)
  {
    DeclarationNameInfo ni = f->getNameInfo();
    SourceRange loc = ni.getSourceRange();
    const string *fname = get_filename(ni.getLoc());
    if (!fname)
      return true;
    const SourceManager &sm = ci.getSourceManager();
    CXXMethodDecl *meth = dynamic_cast<CXXMethodDecl *>(f);
    bool def = f->isThisDeclarationADefinition();
    int type;
    CXXRecordDecl *cls = 0;
    unsigned attr = 0;
    string rettype = f->getReturnType().getAsString(pp);
    string argtypes, argnames;
    parse_arglist(f, &argtypes, &argnames);

    SourceLocation
      begin = ni.getSourceRange().getBegin(),
      end = ni.getSourceRange().getEnd();
    unsigned
      begin_line = sm.getExpansionLineNumber(begin),
      begin_col = sm.getExpansionColumnNumber(begin) - 1,
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end) - 1;
    bool comment = true;
    if (meth) {
      type =  def ? SN_MBR_FUNC_DEF : SN_MBR_FUNC_DCL;
      cls = meth->getParent();
      attr |= access_attr(f->getAccess());
      if (meth->isStatic())
        attr |= SN_STATIC;
      if (meth->isVirtual())
        attr |= meth->isPure() ? SN_PUREVIRTUAL : SN_VIRTUAL;
      if (def && meth->getLexicalDeclContext()->isRecord()) {
        // Sourcenav won't register the member unless it has a DCL.
        // So do both.
        insert_decl(SN_MBR_FUNC_DCL, f, loc, cls, attr, 
                    rettype.c_str(), argtypes.c_str(), argnames.c_str());
        comment = false;
      }
    } else {
      type = def ? SN_FUNC_DEF : SN_FUNC_DCL;
      if (f->getStorageClass() == SC_Static)
        // Gotta love the static keyword!
        attr |= SN_STATIC;
    }
    insert_decl(type, f, loc, cls, attr,
                rettype.c_str(), argtypes.c_str(), argnames.c_str(), comment);
    if (Stmt *bod = f->getBody()) {
      pm.reset(new ParentMap(bod));
      pcls = cls;
      pfun = f;
      pargtypes = argtypes;
    }
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
    } else
      return true;
    insert_decl(type, var, true, cls, attr);
    return true;
  }

  bool
  Sn_ast_visitor::VisitTypedefNameDecl(TypedefNameDecl *tdef)
  {
    DeclContext *ctx = tdef->getDeclContext();
    NamedDecl *cls = 0;
    unsigned attr = 0;
    if (tdef->isCXXClassMember()) {
      cls = static_cast<TagDecl *>(ctx);
      attr |= access_attr(tdef->getAccess());
    }
    insert_decl(SN_TYPE_DEF, tdef, true, cls, attr);
    return true;
  }

  bool
  Sn_ast_visitor::VisitDeclRefExpr(DeclRefExpr *expr)
  {
    xref_decl(expr->getDecl(), expr);
    return true;
  }

  bool
  Sn_ast_visitor::VisitMemberExpr(MemberExpr *expr)
  {
    xref_decl(expr->getMemberDecl(), expr);
    return true;
  }

  bool
  Sn_ast_visitor::VisitDeclStmt(DeclStmt *ds)
  {
    for (auto it = ds->decl_begin(); it != ds->decl_end(); ++it)
      if (DeclaratorDecl *decl = dynamic_cast<DeclaratorDecl *>(*it))
        xref_type(*decl->getType(), ds, decl->getTypeSpecStartLoc());
  }

  void
  Sn_ast_visitor::parse_arglist(
    const FunctionDecl *f, string *argtypes, string *argnames) const
  {
    argtypes->clear();
    if (argnames)
      argnames->clear();
    for (auto it = f->parameters().begin(); 
         it != f->parameters().end(); ++it) {
      const ParmVarDecl &par = **it;
      *argtypes += par.getType().getAsString(pp);
      *argtypes += ",";
      if (argnames) {
        *argnames += par.getName();
        *argnames += ",";
      }
    }
    if (!argtypes->empty())
      argtypes->pop_back();
    if (argnames && !argnames->empty())
      argnames->pop_back();
  }

  bool
  Sn_ast_visitor::insert_decl(
    int sntype, NamedDecl *decl, const SourceRange &loc, const NamedDecl *cls,
    unsigned attr, const char *rettype, const char *argtypes,
    const char *argnames, bool comment) const
  {
    const string *fname = get_filename(loc);
    if (!fname)
      return true;
    attr |= access_attr(decl->getAccess());
    const SourceManager &sm = ci.getSourceManager();
    // Doesn't look like we can find the exact location of the name.
    SourceLocation
      begin = loc.getBegin(),
      end = loc.getEnd();
    unsigned
      begin_line = sm.getExpansionLineNumber(begin),
      begin_col = sm.getExpansionColumnNumber(begin) - 1,
      end_line = sm.getExpansionLineNumber(end),
      end_col = sm.getExpansionColumnNumber(end) - 1;
    sn_insert_symbol(
      sntype, cls ? unsafe_cstr(cls->getName()) : 0, 
      unsafe_cstr(decl->getName()), unsafe_cstr(*fname),
      begin_line, begin_col, end_line, end_col, attr,
      const_cast<char *>(rettype), const_cast<char *>(argtypes),
      const_cast<char *>(argnames),
      comment ? const_cast<char *>(doc_comment(decl)) : 0,
      begin_line, begin_col, end_line, end_col);
    return true;
  }

  int
  Sn_ast_visitor::xref_type(const Type &ty, Stmt *refr, SourceLocation loc)
    const
  {
    const int acc = SN_REF_READ;
    //C.f. QualType::getBaseTypeIdentifier.
    if (ty.isRecordType()) {
      RecordDecl *decl = ty.getAs<RecordType>()->getDecl();
      return xref_decl(decl->isUnion() ? SN_REF_TO_UNION : SN_REF_TO_CLASS,
                       acc, decl, refr, loc);
    } else if (ty.isEnumeralType())
      return xref_decl(SN_REF_TO_ENUM, acc, ty.getAs<EnumType>()->getDecl(),
                       refr, loc);
    else if (ty.getTypeClass() == Type::Typedef)
      return xref_decl(SN_REF_TO_TYPEDEF, acc,
                       ty.getAs<TypedefType>()->getDecl(), refr, loc);
    else if (ty.isPointerType() || ty.isReferenceType())
      return xref_type(*ty.getPointeeType(), refr, loc);
    else if (ty.isArrayType())
      return xref_type(*ty.castAsArrayTypeUnsafe()->getElementType(), 
                       refr, loc);
    else
      return 0;
  }

  int
  Sn_ast_visitor::xref_decl(NamedDecl *decl, Stmt *refr) const
  {
    int sntype;
    unique_ptr<string> argtypes;
    if (auto meth = dynamic_cast<const CXXMethodDecl *>(decl)) {
      sntype = SN_REF_TO_MBR_FUNC;
      argtypes.reset(new string);
      parse_arglist(meth, argtypes.get());
    } else if (auto fun = dynamic_cast<const FunctionDecl *>(decl)) {
      sntype = SN_REF_TO_FUNCTION;
      argtypes.reset(new string);
      parse_arglist(fun, argtypes.get());
    } else
      return true;
    xref_decl(sntype, argtypes ? SN_REF_READ : SN_REF_PASS, decl, refr,
              refr->getLocStart(), argtypes ? argtypes->c_str() : 0);
    return true;
  }

  int
  Sn_ast_visitor::xref_decl(
    int snreftype, int acc, NamedDecl *decl, Stmt *refr,
    SourceLocation loc, const char *argtypes) const
  {
    if (!pm || !pm->hasParent(refr))
      return 0;
    const string *fname = get_filename(loc);
    if (!fname)
      return 0;
    DeclContext *ctx = decl->getDeclContext();
    int lvl = (ctx->isFunctionOrMethod()
               ? SN_REF_SCOPE_LOCAL : SN_REF_SCOPE_GLOBAL);
    RecordDecl *tgt_cls = (ctx->isRecord() 
                           ? static_cast<RecordDecl *>(ctx) : 0);
    const SourceManager &sm = ci.getSourceManager();
    sn_insert_xref(
      snreftype, pcls ? SN_MBR_FUNC_DEF : SN_FUNC_DEF, lvl,
      pcls ? unsafe_cstr(pcls->getName()) : 0, unsafe_cstr(pfun->getName()),
      unsafe_cstr(pargtypes), tgt_cls ? unsafe_cstr(tgt_cls->getName()) : 0,
      unsafe_cstr(decl->getName()), const_cast<char *>(argtypes),
      unsafe_cstr(*fname), sm.getExpansionLineNumber(loc), acc);
  }

  string
  Sn_ast_visitor::simple_typename(const QualType &qt) const
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
      SourceLocation loc = range.getBegin();
      unsigned line = sm.getSpellingLineNumber(loc);
      /* Use of SN_GLOBAL_NAMESPACE is not documented but looks like it is
         supposed to work like this.  The preprocessor cannot know which
         function the reference is in! */
      sn_insert_xref(
        SN_REF_TO_DEFINE, SN_GLOBAL_NAMESPACE, SN_REF_SCOPE_GLOBAL, 0, 0, 0, 0,
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
