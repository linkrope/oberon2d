module oberon2d;

import ast;
import parser;
import scanner : Token;
import std.algorithm;
import std.array;
import std.exception;
import std.string;
import std.typecons;

int main(string[] args)
{
    import std.file : readText;
    import std.stdio : stderr, stdout;

    try
    {
        const text = readText(args[1]);
        auto parser = Parser(text);
        Module module_ = parser.readModule;
        module_.accept(formatter(stdout.lockingTextWriter));
    }
    catch (Exception exception)
    {
        stderr.writeln("error: ", exception.msg);
        return 1;
    }
    return 0;
}

auto formatter(Sink)(Sink sink)
{
    return new Formatter!Sink(sink);
}

class Formatter(Sink) : Visitor
{
    Sink sink;

    this(Sink sink)
    {
        this.sink = sink;
    }

    void visit(Module module_)
    {
        sink.put("module ");
        sink.put(module_.ident);
        sink.put(";\n");
        sink.put("import runtime;\n");
        foreach (import_; module_.imports)
            import_.accept(this);
        foreach (decl; module_.decls)
            decl.accept(this);
        if (!module_.stats.empty)
        {
            sink.put("static this()\n");
            sink.put("{\n");
            foreach (stat; module_.stats)
                stat.accept(this);
            sink.put("}\n");
        }
    }

    void visit(Import import_)
    {
        sink.put("import ");
        if (!import_.aliasIdent.empty)
        {
            sink.put(import_.aliasIdent);
            sink.put(" = ");
        }
        sink.put(import_.ident);
        sink.put(";\n");
    }

    void visit(ConstDecl decl)
    {
        sink.put("const ");
        sink.put(decl.ident);
        sink.put('=');
        decl.expr.accept(this);
        sink.put(";\n");
    }

    void visit(TypeDecl typeDecl)
    {
        auto recordType = cast(RecordType) typeDecl.type;
        if (recordType !is null)
        {
            sink.put("class ");
            sink.put(typeDecl.ident);
            if (!recordType.base.empty)
            {
                sink.put(':');
                sink.put(recordType.base);
            }
            sink.put('\n');
            sink.put("{\n");
            foreach (decl; recordType.decls)
                decl.accept(this);
            sink.put("}\n");
            return;
        }
        sink.put("alias ");
        sink.put(typeDecl.ident);
        sink.put('=');
        typeDecl.type.accept(this);
        sink.put(";\n");
    }

    void visit(VarDecl decl)
    {
        decl.type.accept(this);
        sink.put(' ');
        sink.put(decl.ident);
        sink.put(";\n");
    }

    void visit(ProcDecl procDecl)
    {
        if (!procDecl.comment.empty)
        {
            sink.put("/**\n");
            sink.put("* ");
            const comment = procDecl.comment
                .splitLines
                .map!(line => line.stripLeft)
                .join('\n');
            sink.put(comment);
            sink.put("*/\n");
        }
        if (procDecl.type.empty)
            sink.put("void");
        else
            sink.put(procDecl.type.toType);
        sink.put(' ');
        sink.put(procDecl.ident);
        sink.put('(');
        foreach (count, parameter; procDecl.parameters)
        {
            if (count > 0)
                sink.put(',');
            if (parameter.var)
                sink.put("ref ");
            parameter.type.accept(this);
            sink.put(' ');
            sink.put(parameter.ident);
        }
        sink.put(")\n");
        sink.put("{\n");
        foreach (decl; procDecl.decls)
            decl.accept(this);
        foreach (stat; procDecl.stats)
            stat.accept(this);
        sink.put("}\n");
    }

    void visit(ArrayType type)
    {
        type.type.accept(this);
        if (type.expr is null)
            sink.put("[]");
        else
        {
            sink.put('[');
            type.expr.accept(this);
            sink.put(']');
        }
    }

    void visit(IdentType type)
    {
        sink.put(type.ident.toType);
    }

    void visit(RecordType)
    {
        throw new Exception("unsupported anonymous RECORD type");
    }

    void visit(AssignStat stat)
    {
        stat.lhs.accept(this);
        sink.put('=');
        stat.rhs.accept(this);
        sink.put(";\n");
    }

    void visit(CaseStat caseStat)
    {
        foreach (caseRange; caseStat.caseRanges)
        {
            sink.put("case ");
            caseRange.first.accept(this);
            if (caseRange.last !is null)
            {
                sink.put(": .. case ");
                caseRange.last.accept(this);
            }
            sink.put(":\n");
        }
        foreach (stat; caseStat.stats)
            stat.accept(this);
        sink.put("break;");
    }

    void visit(CallStat stat)
    {
        stat.expr.accept(this);
        sink.put(";\n");
    }

    void visit(DoWhileStat doWhileStat)
    {
        sink.put("do\n");
        sink.put("{\n");
        foreach (stat; doWhileStat.stats)
            stat.accept(this);
        sink.put("} while (");
        doWhileStat.expr.accept(this);
        sink.put(");\n");
    }

    void visit(ExitStat)
    {
        sink.put("break;\n");
    }

    void visit(ForStat forStat)
    {
        sink.put("for (");
        forStat.initialize.accept(this);
        sink.put(';');
        forStat.test.accept(this);
        sink.put(';');
        forStat.increment.accept(this);
        sink.put(")\n");
        sink.put("{\n");
        foreach (stat; forStat.stats)
            stat.accept(this);
        sink.put("}\n");
    }

    void visit(IfElseStat ifElseStat)
    {
        foreach (count, ifStat; ifElseStat.ifStats)
        {
            if (count == 0)
                sink.put("if (");
            else
                sink.put("else if (");
            ifStat.expr.accept(this);
            sink.put(")\n");
            sink.put("{\n");
            foreach (stat; ifStat.stats)
                stat.accept(this);
            sink.put("}\n");
        }
        if (!ifElseStat.stats.empty)
        {
            sink.put("else\n");
            sink.put("{\n");
            foreach (stat; ifElseStat.stats)
                stat.accept(this);
            sink.put("}\n");
        }
    }

    void visit(ReturnStat stat)
    {
        if (stat.expr is null)
            sink.put("return;");
        else
        {
            sink.put("return ");
            stat.expr.accept(this);
            sink.put(';');
        }
    }

    void visit(SwitchStat switchStat)
    {
        sink.put("switch (");
        switchStat.expr.accept(this);
        sink.put(")\n");
        sink.put("{\n");
        foreach (caseStat; switchStat.caseStats)
            visit(caseStat);
        if (!switchStat.stats.empty)
        {
            sink.put("default:\n");
            foreach (stat; switchStat.stats)
                stat.accept(this);
        }
        sink.put("}\n");
    }

    void visit(WhileStat whileStat)
    {
        sink.put("while (");
        whileStat.expr.accept(this);
        sink.put(")\n");
        sink.put("{\n");
        foreach (stat; whileStat.stats)
            stat.accept(this);
        sink.put("}\n");
    }

    void visit(WithElseStat withElseStat)
    {
        foreach (withStat; withElseStat.withStats)
        {
            sink.put("// TODO WITH ");
            sink.put(withStat.ident);
            sink.put(" : ");
            sink.put(withStat.type);
            sink.put(" DO\n");
            foreach (stat; withStat.stats)
                stat.accept(this);
        }
        if (!withElseStat.stats.empty)
        {
            sink.put("// TODO ELSE\n");
            foreach (stat; withElseStat.stats)
                stat.accept(this);
        }
    }

    void visit(BinaryExpr expr)
    {
        if (expr.op == Token.DIV || expr.op == Token.MOD)
        {
            visit(new CallExpr(new PrimaryExpr(expr.op), [expr.lhs, expr.rhs]));
            return;
        }
        parenthesize(expr.precedence, expr.lhs);
        sink.put(' ');
        sink.put(expr.op.toOp);
        sink.put(' ');
        parenthesize(expr.precedence, expr.rhs);
    }

    void visit(CallExpr expr)
    {
        auto primaryExpr = cast(PrimaryExpr) expr.expr;
        if (primaryExpr !is null)
        {
            const name = primaryExpr.value;
            if ((name == "INC" || name == "DEC") && expr.exprs.length == 1)
            {
                sink.put((name == "INC") ? "++" : "--");
                expr.exprs.front.accept(this);
                return;
            }
            else if (name == "MIN" || name == "MAX")
            {
                enforce(!expr.exprs.empty, "missing argument");
                primaryExpr = cast(PrimaryExpr) expr.exprs.front;
                enforce(primaryExpr !is null, "basic type expected");
                const type = primaryExpr.value;
                if (type == "SET")
                {
                    sink.put((name == "MIN") ? "0" : "31");
                }
                else
                {
                    sink.put(type.toType);
                    sink.put('.');
                    sink.put((name == "MIN") ? "min" : "max");
                }
                return;
            }
            else if (name == "LEN")
            {
                enforce(!expr.exprs.empty, "missing argument");
                expr.exprs.front.accept(this);
                sink.put(".length");
                return;
            }
        }
        expr.expr.accept(this);
        sink.put('(');
        foreach (count, arg; expr.exprs)
        {
            if (count > 0)
                sink.put(", ");
            arg.accept(this);
        }
        sink.put(')');
    }

    void visit(FieldExpr expr)
    {
        expr.expr.accept(this);
        sink.put('.');
        sink.put(expr.field);
    }

    void visit(IndexExpr expr)
    {
        expr.expr.accept(this);
        sink.put('[');
        expr.index.accept(this);
        sink.put(']');
    }

    void visit(PrimaryExpr expr)
    {
        sink.put(expr.value);
    }

    void visit(UnaryExpr expr)
    {
        sink.put(expr.op.toOp);
        parenthesize(expr.precedence, expr.expr);
    }

    void parenthesize(Precedence precedence, Expr expr)
    {
        if (precedence > expr.precedence)
        {
            sink.put('(');
            expr.accept(this);
            sink.put(')');
        }
        else
            expr.accept(this);
    }
}

string toType(string ident)
{
    switch (ident)
    {
    case "BOOLEAN":
        return "bool";
    case "CHAR":
        return "char";
    case "SHORTINT":
        return "short";
    case "INTEGER":
        return "int";
    case "LONGINT":
        return "long";
    case "REAL":
        return "float";
    case "LONGREAL":
        return "double";
    case "SET":
        return "uint";
    default:
        return ident;
    }
}

Precedence precedence(Expr expr)
{
    auto evaluator = new Evaluator;
    expr.accept(evaluator);
    return evaluator.precedence;
}

enum Precedence
{
    assign,
    or_or,
    and_and,
    cmp,
    add,
    mul,
    unary,
}

class Evaluator : BlackHole!Visitor
{
    Precedence precedence = Precedence.unary;

    override void visit(BinaryExpr expr)
    {
        if (expr.op == Token.DIV || expr.op == Token.MOD)
        {
            precedence = Precedence.unary;
            return;
        }
        switch (expr.op.toOp) with (Precedence)
        {
        case "=":
            precedence = assign;
            break;
        case "||":
            precedence = or_or;
            break;
        case "&&":
            precedence = and_and;
            break;
        case "==":
        case "!=":
        case "<":
        case "<=":
        case ">":
        case ">=":
        case "in":
        case "is":
            precedence = cmp;
            break;
        case "+":
        case "-":
            precedence = add;
            break;
        case "*":
        case "/":
            precedence = mul;
            break;
        default:
            assert(false, `unexpected operator "` ~ expr.op ~ `"`);
        }
    }
}

string toOp(string op)
{
    switch (op) with (Token)
    {
    case and:
        return "&&";
    case becomes:
        return "=";
    case equal:
        return "==";
    case not:
        return "!";
    case notEqual:
        return "!=";
    case DIV:
        return "/";
    case IN:
        return "in";
    case IS:
        return "is";
    case MOD:
        return "%";
    case OR:
        return "||";
    default:
        return op;
    }
}
