module parser;

import ast;
import scanner;
import std.array;
import std.exception;
import std.typecons;

struct Parser
{
    Scanner scanner;

    this(string text)
    {
        scanner = Scanner(text);
    }

    // module = MODULE ident ";" [ImportList] DeclarationSequence
    //     [BEGIN StatementSequence] END ident ".".
    // ImportList = IMPORT import ["," import] ";".
    // import = ident [":=" ident].
    Module readModule()
    {
        check(Token.MODULE);
        enforce(scanner.front == Token.ident, "identifier expected");
        string ident = scanner.value;
        scanner.popFront;
        check(Token.semicolon);
        Import[] imports;
        if (scanner.front == Token.IMPORT)
        {
            scanner.popFront;
            while (scanner.front == Token.ident)
            {
                string importIdent = scanner.value;
                string aliasIdent;
                scanner.popFront;
                if (scanner.front == Token.becomes)
                {
                    scanner.popFront;
                    enforce(scanner.front == Token.ident, "identifier expected");
                    aliasIdent = importIdent;
                    importIdent = scanner.value;
                    scanner.popFront;
                }
                if (scanner.front == Token.comma)
                    scanner.popFront;
                else
                    enforce(scanner.front != Token.ident, ", missing");
                imports ~= new Import(importIdent, aliasIdent);
            }
            check(Token.semicolon);
        }
        Decl[] decls = readDeclarationSequence;
        Stat[] stats;
        if (scanner.front == Token.BEGIN)
        {
            scanner.popFront;
            stats = readStatementSequence;
        }
        check(Token.END);
        enforce(scanner.front == Token.ident, "identifier expected");
        scanner.popFront;
        check(Token.period);
        return new Module(ident, imports, decls, stats);
    }

    // DeclarationSequence = {CONST {ConstDeclaration ";"}
    //     | TYPE {TypeDeclaration ";"}
    //     | VAR {VariableDeclaration";"}}
    //     {ProcedureDeclaration ";" | ForwardDeclaration ";"}.
    // ConstDeclaration = identdef "=" ConstExpression.
    // TypeDeclaration = identdef "=" type.
    // VariableDeclaration = IdentList ":" type.
    Decl[] readDeclarationSequence()
    {
        Decl[] soFar;
        while (true)
            if (scanner.front == Token.CONST)
            {
                scanner.popFront;
                while (scanner.front == Token.ident)
                {
                    string ident = scanner.value;
                    scanner.popFront;
                    checkExport;
                    check(Token.equal);
                    Expr expr = readExpression;
                    check(Token.semicolon);
                    soFar ~= new ConstDecl(ident, expr);
                }
            }
            else if (scanner.front == Token.TYPE)
            {
                scanner.popFront;
                while (scanner.front == Token.ident)
                {
                    string ident = scanner.value;
                    scanner.popFront;
                    checkExport;
                    check(Token.equal);
                    Type type = readType;
                    check(Token.semicolon);
                    soFar ~= new TypeDecl(ident, type);
                }
            }
            else if (scanner.front == Token.VAR)
            {
                scanner.popFront;
                while (scanner.front == Token.ident)
                {
                    string[] identList = readIdentList;
                    check(Token.colon);
                    Type type = readType;
                    check(Token.semicolon);
                    foreach (ident; identList)
                        soFar ~= new VarDecl(ident, type);
                }
            }
            else
                break;
        while (scanner.front == Token.PROCEDURE)
        {
            soFar ~= readProcedureDeclaration;
            check(Token.semicolon);
        }
        return soFar;
    }

    // ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
    // ProcedureHeading = PROCEDURE [Receiver] identdef [FormalParameters].
    // ProcedureBody = DeclarationSequence [BEGIN StatementSequence] END.
    // ForwardDeclaration = PROCEDURE "^" [Receiver] identdef [FormalParameters].
    // Receiver = "(" [VAR] ident ":" ident ")".
    ProcDecl[] readProcedureDeclaration()
    in
    {
        assert(scanner.front == Token.PROCEDURE);
    }
    body
    {
        bool forwardDecl = false;
        scanner.popFront;
        if (scanner.front == Token.arrow)
        {
            scanner.popFront;
            forwardDecl = true;
        }
        enforce(scanner.front != Token.leftParen, "unsupported type-bound procedure");
        enforce(scanner.front == Token.ident, "identifier expected");
        string ident = scanner.value;
        scanner.popFront;
        checkExport;
        FormalPars formalPars = readFormalParameters;
        if (forwardDecl)
            return null;
        check(Token.semicolon);
        string comment = scanner.comment;
        Decl[] decls = readDeclarationSequence;
        Stat[] stats;
        if (scanner.front == Token.BEGIN)
        {
            scanner.popFront;
            stats = readStatementSequence;
        }
        check(Token.END);
        enforce(scanner.front == Token.ident, "identifier expected");
        scanner.popFront;
        return [new ProcDecl(comment, ident, formalPars.parameters, formalPars.type, decls, stats)];
    }

    alias FormalPars = Tuple!(Parameter[], "parameters", string, "type");

    // FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
    FormalPars readFormalParameters()
    {
        Parameter[] parameters;
        string type;
        if (scanner.front == Token.leftParen)
        {
            scanner.popFront;
            if (scanner.front != Token.rightParen)
            {
                parameters ~= readFPSection;
                while (scanner.front == Token.semicolon)
                {
                    scanner.popFront;
                    parameters ~= readFPSection;
                }
            }
            check(Token.rightParen);
            if (scanner.front == Token.colon)
            {
                scanner.popFront;
                type = readQualident;
            }
        }
        return FormalPars(parameters, type);
    }

    // FPSection = [VAR] ident {"," ident} ":" type.
    Parameter[] readFPSection()
    {
        const var = scanner.front == Token.VAR;
        if (scanner.front == Token.VAR)
            scanner.popFront;
        string[] identList = readIdentList;
        check(Token.colon);
        Type type = readType;
        Parameter[] parameters;
        foreach (ident; identList)
            parameters ~= Parameter(var, ident, type);
        return parameters;
    }

    // type = qualident | ArrayType | RecordType | PointerType | ProcedureType.
    // ArrayType = ARRAY [length {"," length}] OF type.
    // length = ConstExpression
    // RecordType = RECORD ["("BaseType")"] [FieldListSequence] END.
    // BaseType = qualident.
    // FieldListSequence = FieldList {";" FieldList}.
    // FieldList = IdentList ":" type.
    // PointerType = POINTER TO type.
    // ProcedureType = PROCEDURE [FormalParameters].
    Type readType()
    {
        if (scanner.front == Token.ident)
        {
            string ident = readQualident;
            return new IdentType(ident);
        }
        else if (scanner.front == Token.ARRAY)
        {
            Expr[] exprs;
            scanner.popFront;
            if (scanner.front != Token.OF)
            {
                exprs ~= readExpression;
                while (scanner.front == Token.comma)
                {
                    scanner.popFront;
                    exprs ~=readExpression;
                }
            }
            check(Token.OF);
            Type type = readType;
            if (exprs.empty)
                type = new ArrayType(type);
            else
                foreach_reverse (expr; exprs)
                    type = new ArrayType(expr, type);
            return type;
        }
        else if (scanner.front == Token.RECORD)
        {
            string base;
            check(Token.RECORD);
            if (scanner.front == Token.leftParen)
            {
                scanner.popFront;
                base = readQualident;
                check(Token.rightParen);
            }
            Decl[] decls;
            while (scanner.front == Token.ident)
            {
                string[] identList = readIdentList;
                check(Token.colon);
                Type type = readType;
                foreach (ident; identList)
                    decls ~= new VarDecl(ident, type);
                if (scanner.front == Token.semicolon)
                    scanner.popFront;
                else
                    break;
            }
            check(Token.END);
            return new RecordType(base, decls);
        }
        else if (scanner.front == Token.POINTER)
        {
            scanner.popFront;
            check(Token.TO);
            Type type = readType;
            return type;
        }
        else if (scanner.front == Token.PROCEDURE)
            throw new Exception("unsupported PROCEDURE type");
        else
            throw new Exception("illegal type");
    }

    // StatementSequence = statement {";" statement}.
    // statement = [assignment | ProcedureCall | IfStatement | CaseStatement
    //     | WhileStatement | RepeatStatement | ForStatement
    //     | LoopStatement | WithStatement | EXIT | RETURN [expression]].
    // assignment = designator ":=" expression.
    // ProcedureCall = designator [ActualParameters].
    // IfStatement = IF expression THEN StatementSequence
    //     {ELSIF expression THEN StatementSequence}
    //     [ELSE StatementSequence]
    //     END.
    // CaseStatement = CASE expression OF case {"|" case}
    //     [ELSE StatementSequence] END.
    // WhileStatement = WHILE expression DO StatementSequence
    //     {ELSIF expression DO StatementSequence} END.
    // RepeatStatement = REPEAT StatementSequence UNTIL expression.
    // ForStatement = FOR ident ":=" expression TO expression
    //     [BY ConstExpression] DO StatementSequence END.
    // LoopStatement = LOOP StatementSequence END.
    // WithStatement = WITH Guard DO StatementSequence
    //     {"|" Guard DO StatementSequence}
    //     [ELSE StatementSequence] END.
    // Guard = qualident ":" qualident.
    Stat[] readStatementSequence()
    {
        Stat[] soFar;
        while (true)
            if (scanner.front == Token.ident)
            {
                Expr expr = readDesignator;
                if (scanner.front == Token.becomes)
                {
                    scanner.popFront;
                    Expr rhs = readExpression;
                    soFar ~= new AssignStat(expr, rhs);
                }
                else
                    soFar ~= new CallStat(expr);
            }
            else if (scanner.front == Token.IF)
            {
                IfStat[] ifStats;
                scanner.popFront;
                Expr expr = readExpression;
                check(Token.THEN);
                Stat[] stats = readStatementSequence;
                ifStats ~= IfStat(expr, stats);
                while (scanner.front == Token.ELSIF)
                {
                    scanner.popFront;
                    expr = readExpression;
                    check(Token.THEN);
                    stats = readStatementSequence;
                    ifStats ~= IfStat(expr, stats);
                }
                if (scanner.front == Token.ELSE)
                {
                    scanner.popFront;
                    stats = readStatementSequence;
                }
                else
                    stats = null;
                check(Token.END);
                soFar ~= new IfElseStat(ifStats, stats);
            }
            else if (scanner.front == Token.CASE)
            {
                CaseStat[] caseStats;
                scanner.popFront;
                Expr expr = readExpression;
                check(Token.OF);
                caseStats ~= readCase;
                while (scanner.front == Token.bar)
                {
                    scanner.popFront;
                    caseStats ~= readCase;
                }
                Stat[] stats;
                if (scanner.front == Token.ELSE)
                {
                    scanner.popFront;
                    stats = readStatementSequence;
                }
                check(Token.END);
                soFar ~= new SwitchStat(expr, caseStats, stats);
            }
            else if (scanner.front == Token.WHILE)
            {
                scanner.popFront;
                Expr expr = readExpression;
                check(Token.DO);
                Stat[] stats = readStatementSequence;
                // TODO {ELSIF expression DO StatementSequence}
                check(Token.END);
                soFar ~= new WhileStat(expr, stats);
            }
            else if (scanner.front == Token.REPEAT)
            {
                scanner.popFront;
                Stat[] stats = readStatementSequence;
                check(Token.UNTIL);
                Expr expr = readExpression;
                soFar ~= new DoWhileStat(stats, new UnaryExpr(Token.not, expr));
            }
            else if (scanner.front == Token.FOR)
            {
                scanner.popFront;
                enforce(scanner.front == Token.ident, "identifier expected");
                Expr ident = new PrimaryExpr(scanner.value);
                scanner.popFront;
                check(Token.becomes);
                Expr begin = readExpression;
                Expr initialize = new BinaryExpr(Token.becomes, ident, begin);
                check(Token.TO);
                Expr end = readExpression;
                Expr test = new BinaryExpr(Token.lessEqual, ident, end);
                Expr increment;
                if (scanner.front == Token.BY)
                {
                    scanner.popFront;
                    Expr step = readExpression;
                    increment = new BinaryExpr(Token.becomes, ident, new BinaryExpr(Token.plus, ident, step));
                }
                else
                    increment = new CallExpr(new PrimaryExpr("INC"), [ident]);
                check(Token.DO);
                Stat[] stats = readStatementSequence;
                check(Token.END);
                soFar ~= new ForStat(initialize, test, increment, stats);
            }
            else if (scanner.front == Token.LOOP)
            {
                scanner.popFront;
                Stat[] stats = readStatementSequence;
                check(Token.END);
                soFar ~= new WhileStat(new PrimaryExpr("true"), stats);
            }
            else if (scanner.front == Token.WITH)
            {
                WithStat[] withStats;
                scanner.popFront;
                while (true)
                {
                    string ident = readQualident;
                    check(Token.colon);
                    string type = readQualident;
                    check(Token.DO);
                    Stat[] stats = readStatementSequence;
                    withStats ~= WithStat(ident, type, stats);
                    if (scanner.front != Token.bar)
                        break;
                    scanner.popFront;
                }
                Stat[] stats;
                if (scanner.front == Token.ELSE)
                {
                    scanner.popFront;
                    stats = readStatementSequence;
                }
                check(Token.END);
                soFar ~= new WithElseStat(withStats, stats);
            }
            else if (scanner.front == Token.EXIT)
            {
                scanner.popFront;
                soFar ~= new ExitStat;
            }
            else if (scanner.front == Token.RETURN)
            {
                Expr expr;
                scanner.popFront;
                if (scanner.front != Token.semicolon && scanner.front != Token.bar &&
                    scanner.front != Token.ELSE && scanner.front != Token.ELSIF &&
                    scanner.front != Token.END && scanner.front != Token.UNTIL)
                    expr = readExpression;
                soFar ~= new ReturnStat(expr);
            }
            else if (scanner.front == Token.semicolon)
                scanner.popFront;
            else
                break;
        return soFar;
    }

    // case = [CaseLabelList ":" StatementSequence].
    // CaseLabelList = CaseLabels {"," CaseLabels}.
    // CaseLabels = ConstExpression [".." ConstExpression].
    CaseStat[] readCase()
    {
        if (scanner.front != Token.bar &&
            scanner.front != Token.ELSE && scanner.front != Token.END)
        {
            CaseRange[] caseRanges;
            while (true)
            {
                Expr first = readExpression;
                Expr last;
                if (scanner.front == Token.upTo)
                {
                    scanner.popFront;
                    last = readExpression;
                }
                caseRanges ~= CaseRange(first, last);
                if (scanner.front != Token.comma)
                    break;
                scanner.popFront;
            }
            check(Token.colon);
            Stat[] stats = readStatementSequence;
            return [new CaseStat(caseRanges, stats)];
        }
        return null;
    }

    // ConstExpression = expression.
    // expression = SimpleExpression [relation SimpleExpression].
    // relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
    Expr readExpression()
    {
        Expr expr = readSimpleExpression;
        if (scanner.front == Token.equal || scanner.front == Token.notEqual ||
            scanner.front == Token.less || scanner.front == Token.lessEqual ||
            scanner.front == Token.greater || scanner.front == Token.greaterEqual ||
            scanner.front == Token.IN || scanner.front == Token.IS)
        {
            const token = scanner.front;
            scanner.popFront;
            expr = new BinaryExpr(token, expr, readSimpleExpression);
        }
        return expr;
    }

    // SimpleExpression = ["+" | "-"] term {AddOperator term}.
    // AddOperator = "+" | "-" | OR.
    Expr readSimpleExpression()
    {
        Expr expr;
        if (scanner.front == Token.plus || scanner.front == Token.minus)
        {
            const token = scanner.front;
            scanner.popFront;
            expr = new UnaryExpr(token, readTerm);
        }
        else
            expr = readTerm;
        while (scanner.front == Token.plus || scanner.front == Token.minus ||
            scanner.front == Token.OR)
        {
            const token = scanner.front;
            scanner.popFront;
            expr = new BinaryExpr(token, expr, readTerm);
        }
        return expr;
    }

    // term = factor {MulOperator factor}.
    // MulOperator = "*" | "/" | DIV | MOD | "&".
    Expr readTerm()
    {
        Expr expr = readFactor;
        while (scanner.front == Token.times || scanner.front == Token.slash ||
            scanner.front == Token.DIV || scanner.front == Token.MOD ||
            scanner.front == Token.and)
        {
            const token = scanner.front;
            scanner.popFront;
            expr = new BinaryExpr(token, expr, readFactor);
        }
        return expr;
    }

    // factor = number | character | string | NIL | TRUE | FALSE
    //     | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
    // ActualParameters "(" [ExpList] ")".
    Expr readFactor()
    {
        Expr expr;
        if (scanner.front == Token.ident)
            expr = readDesignator;
        else if (scanner.front == Token.integer)
        {
            expr = new PrimaryExpr(scanner.value);
            scanner.popFront;
        }
        else if (scanner.front == Token.real_)
        {
            expr = new PrimaryExpr(scanner.value);
            scanner.popFront;
        }
        else if (scanner.front == Token.character)
        {
            expr = new PrimaryExpr("'" ~ scanner.value ~ "'");
            scanner.popFront;
        }
        else if (scanner.front == Token.NIL)
        {
            expr = new PrimaryExpr("null");
            scanner.popFront;
        }
        else if (scanner.front == Token.FALSE)
        {
            expr = new PrimaryExpr("false");
            scanner.popFront;
        }
        else if (scanner.front == Token.TRUE)
        {
            expr = new PrimaryExpr("true");
            scanner.popFront;
        }
        else if (scanner.front == Token.string_)
        {
            expr = new PrimaryExpr('"' ~ scanner.value ~ '"');
            scanner.popFront;
        }
        else if (scanner.front == Token.leftParen)
        {
            scanner.popFront;
            expr = readExpression;
            check(Token.rightParen);
        }
        else if (scanner.front == Token.leftBrace)
            expr = readSet;
        else if (scanner.front == Token.not)
        {
            const token = scanner.front;
            scanner.popFront;
            expr = new UnaryExpr(token, readFactor);
        }
        return expr;
    }

    // set = {" [element {"," element}] "}".
    Expr readSet()
    {
        Expr expr = new PrimaryExpr("Set");
        check(Token.leftBrace);
        if (scanner.front != Token.rightBrace)
        {
            readElement;
            while (scanner.front == Token.comma)
            {
                scanner.popFront;
                readElement;
            }
        }
        check(Token.rightBrace);
        return expr;
    }

    // element = expression [".." expression].
    void readElement()
    {
        readExpression;
        if (scanner.front == Token.upTo)
        {
            scanner.popFront;
            readExpression;
        }
    }

    // designator = qualident {selector}.
    // selector = "." ident | "[" ExpList "]" | " ^ " | "(" qualident ")".
    Expr readDesignator()
    {
        string ident = readQualident;
        Expr expr = new PrimaryExpr(ident);
        // avoiding ambiguity of "(" qualident ")"
        while (true)
            if (scanner.front == Token.leftBracket)
            {
                scanner.popFront;
                Expr[] exprList = readExpList;
                check(Token.rightBracket);
                foreach (index; exprList)
                    expr = new IndexExpr(expr, index);
            }
            else if (scanner.front == Token.period)
            {
                scanner.popFront;
                enforce(scanner.front == Token.ident, "identifier expected");
                string field = scanner.value;
                scanner.popFront;
                expr = new FieldExpr(expr, field);
            }
            else if (scanner.front == Token.arrow)
                scanner.popFront;
            else if (scanner.front == Token.leftParen)
            {
                scanner.popFront;
                Expr[] exprList;
                if (scanner.front != Token.rightParen)
                    exprList = readExpList;
                check(Token.rightParen);
                expr = new CallExpr(expr, exprList);
            }
            else
                break;
        return expr;
    }

    // ExpList = = expression {"," expression}.
    Expr[] readExpList()
    {
        Expr[] expList;
        expList ~= readExpression;
        while (scanner.front == Token.comma)
        {
            scanner.popFront;
            expList ~= readExpression;
        }
        return expList;
    }

    // IdentList = identdef {"," identdef}.
    string[] readIdentList()
    {
        string[] identList;
        enforce(scanner.front == Token.ident, "identifier expected");
        identList ~= scanner.value;
        scanner.popFront;
        checkExport;
        while (scanner.front == Token.comma)
        {
            scanner.popFront;
            enforce(scanner.front == Token.ident, "identifier expected");
            identList ~= scanner.value;
            scanner.popFront;
            checkExport;
        }
        return identList;
    }

    // qualident = [ident "."] ident.
    string readQualident()
    {
        string[] soFar;
        enforce(scanner.front == Token.ident, "identifier expected");
        soFar ~= scanner.value;
        scanner.popFront;
        if (scanner.front == Token.period)
        {
            scanner.popFront;
            enforce(scanner.front == Token.ident, "identifier expected");
            soFar ~= scanner.value;
            scanner.popFront;
        }
        return soFar.join('.');
    }

    // identdef = ident ["*"|"-"].
    void checkExport()
    {
        if (scanner.front == Token.times)
            scanner.popFront;
        enforce(scanner.front != Token.minus, "unsupported read-only field");
    }

    void check(Token token)
    {
        enforce(scanner.front == token, token ~ " missing");
        scanner.popFront;
    }
}
