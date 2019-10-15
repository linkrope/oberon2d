module ast;

import scanner;

interface Visitor
{
    void visit(Module module_);
    void visit(Import import_);
    void visit(ConstDecl decl);
    void visit(TypeDecl decl);
    void visit(VarDecl decl);
    void visit(ProcDecl decl);
    void visit(ArrayType type);
    void visit(IdentType type);
    void visit(RecordType type);
    void visit(AssignStat stat);
    void visit(CallStat stat);
    void visit(DoWhileStat stat);
    void visit(ExitStat stat);
    void visit(ForStat stat);
    void visit(IfElseStat stat);
    void visit(ReturnStat stat);
    void visit(SwitchStat stat);
    void visit(WhileStat stat);
    void visit(WithElseStat stat);
    void visit(BinaryExpr expr);
    void visit(CallExpr expr);
    void visit(FieldExpr expr);
    void visit(IndexExpr expr);
    void visit(PrimaryExpr expr);
    void visit(UnaryExpr expr);
}

class Node
{
    abstract void accept(Visitor visitor);
}

class Module : Node
{
    string ident;
    Import[] imports;
    Decl[] decls;
    Stat[] stats;

    this(string ident, Import[] imports, Decl[] decls, Stat[] stats)
    {
        this.ident = ident;
        this.imports = imports,
        this.decls = decls;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

class Import : Node
{
    string ident;
    string aliasIdent;

    this(string ident, string aliasIdent)
    {
        this.ident = ident;
        this.aliasIdent = aliasIdent;
    }

    mixin Accept!Visitor;
}

class Decl : Node
{
}

class ConstDecl : Decl
{
    string ident;
    Expr expr;

    this(string ident, Expr expr)
    {
        this.ident = ident;
        this.expr = expr;
    }

    mixin Accept!Visitor;
}

class TypeDecl : Decl
{
    string ident;
    Type type;

    this(string ident, Type type)
    {
        this.ident = ident;
        this.type = type;
    }

    mixin Accept!Visitor;
}

class VarDecl : Decl
{
    string ident;
    Type type;

    this(string ident, Type type)
    {
        this.ident = ident;
        this.type = type;
    }

    mixin Accept!Visitor;
}

class ProcDecl : Decl
{
    string comment;
    string ident;
    Parameter[] parameters;
    string type;
    Decl[] decls;
    Stat[] stats;

    this(string comment, string ident, Parameter[] parameters, string type, Decl[] decls, Stat[] stats)
    {
        this.comment = comment;
        this.ident = ident;
        this.parameters = parameters;
        this.type = type;
        this.decls = decls;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

struct Parameter
{
    bool var;
    string ident;
    Type type;

    this(bool var, string ident, Type type)
    {
        this.var = var;
        this.ident = ident;
        this.type = type;
    }
}

class Type : Node
{
}

class ArrayType : Type
{
    Expr expr;
    Type type;

    this(Type type)
    {
        this(null, type);
    }

    this(Expr expr, Type type)
    {
        this.expr = expr;
        this.type = type;
    }

    mixin Accept!Visitor;
}

class IdentType : Type
{
    string ident;

    this(string ident)
    {
        this.ident = ident;
    }

    mixin Accept!Visitor;
}

class RecordType : Type
{
    string base;
    Decl[] decls;

    this(string base, Decl[] decls)
    {
        this.base = base;
        this.decls = decls;
    }

    mixin Accept!Visitor;
}

class Stat : Node
{
}

class AssignStat : Stat
{
    Expr lhs;
    Expr rhs;

    this(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    mixin Accept!Visitor;
}

class CaseStat
{
    CaseRange[] caseRanges;
    Stat[] stats;

    this(CaseRange[] caseRanges, Stat[] stats)
    {
        this.caseRanges = caseRanges;
        this.stats = stats;
    }
}

struct CaseRange
{
    Expr first;
    Expr last;

    this(Expr first, Expr last)
    {
        this.first = first;
        this.last = last;
    }
}

class CallStat : Stat
{
    Expr expr;

    this(Expr expr)
    {
        this.expr = expr;
    }

    mixin Accept!Visitor;
}

class DoWhileStat : Stat
{
    Stat[] stats;
    Expr expr;

    this(Stat[] stats, Expr expr)
    {
        this.stats = stats;
        this.expr = expr;
    }

    mixin Accept!Visitor;
}

class ExitStat : Stat
{
    mixin Accept!Visitor;
}

class ForStat : Stat
{
    Expr initialize;
    Expr test;
    Expr increment;
    Stat[] stats;

    this(Expr initialize, Expr test, Expr increment, Stat[] stats)
    {
        this.initialize = initialize;
        this.test = test;
        this.increment = increment;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

class IfElseStat : Stat
{
    IfStat[] ifStats;
    Stat[] stats;

    this(IfStat[] ifStats, Stat[] stats)
    {
        this.ifStats = ifStats;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

struct IfStat
{
    Expr expr;
    Stat[] stats;

    this(Expr expr, Stat[] stats)
    {
        this.expr = expr;
        this.stats = stats;
    }
}

class ReturnStat : Stat
{
    Expr expr;

    this(Expr expr)
    {
        this.expr = expr;
    }

    mixin Accept!Visitor;
}

class SwitchStat : Stat
{
    Expr expr;
    CaseStat[] caseStats;
    Stat[] stats;

    this(Expr expr, CaseStat[] caseStats, Stat[] stats)
    {
        this.expr = expr;
        this.caseStats = caseStats;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

class WhileStat : Stat
{
    Expr expr;
    Stat[] stats;

    this(Expr expr, Stat[] stats)
    {
        this.expr = expr;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

class WithElseStat : Stat
{
    WithStat[] withStats;
    Stat[] stats;

    this(WithStat[] withStats, Stat[] stats)
    {
        this.withStats = withStats;
        this.stats = stats;
    }

    mixin Accept!Visitor;
}

struct WithStat
{
    string ident;
    string type;
    Stat[] stats;

    this(string ident, string type, Stat[] stats)
    {
        this.ident = ident;
        this.type = type;
        this.stats = stats;
    }
}

class Expr : Node
{
}

class BinaryExpr : Expr
{
    string op;
    Expr lhs;
    Expr rhs;

    this(string op, Expr lhs, Expr rhs)
    {
        this.op = op;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    mixin Accept!Visitor;
}

class CallExpr : Expr
{
    Expr expr;
    Expr[] exprs;

    this(Expr expr, Expr[] exprs)
    {
        this.expr = expr;
        this.exprs = exprs;
    }

    mixin Accept!Visitor;
}

class FieldExpr : Expr
{
    Expr expr;
    string field;

    this(Expr expr, string field)
    {
        this.expr = expr;
        this.field = field;
    }

    mixin Accept!Visitor;
}

class IndexExpr : Expr
{
    Expr expr;
    Expr index;

    this(Expr expr, Expr index)
    {
        this.expr = expr;
        this.index = index;
    }

    mixin Accept!Visitor;
}

class PrimaryExpr : Expr
{
    string value;

    this(string value)
    {
        this.value = value;
    }

    mixin Accept!Visitor;
}

class UnaryExpr : Expr
{
    string op;
    Expr expr;

    this(string op, Expr expr)
    {
        this.op = op;
        this.expr = expr;
    }

    mixin Accept!Visitor;
}

mixin template Accept(Visitor)
{
    override void accept(Visitor visitor)
    {
        visitor.visit(this);
    }
}
