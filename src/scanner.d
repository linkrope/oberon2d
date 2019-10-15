module scanner;

import std.ascii;
import std.exception;
import std.format;
import std.range;

enum Token : string
{
    eof = null,

    and = "&",
    arrow = "^",
    bar = "|",
    becomes = ":=",
    character = "<character>",
    colon = ":",
    comma = ",",
    equal = "=",
    greater = ">",
    greaterEqual = ">=",
    ident = "<ident>",
    integer = "<integer>",
    leftBrace = "{",
    leftBracket = "[",
    less = "<",
    lessEqual = "<=",
    leftParen = "(",
    minus = "-",
    not = "~",
    notEqual = "#",
    period = ".",
    plus = "+",
    rightBrace = "}",
    rightBracket = "]",
    real_ = "<real>",
    rightParen = ")",
    semicolon = ";",
    slash = "/",
    string_ = "<string>",
    times = "*",
    upTo = "..",

    ARRAY = "ARRAY",
    BEGIN = "BEGIN",
    BY = "BY",
    CASE = "CASE",
    CONST = "CONST",
    DIV = "DIV",
    DO = "DO",
    ELSE = "ELSE",
    ELSIF = "ELSIF",
    END = "END",
    EXIT = "EXIT",
    FALSE = "FALSE",
    FOR = "FOR",
    IF = "IF",
    IMPORT = "IMPORT",
    IN = "IN",
    IS = "IS",
    LOOP = "LOOP",
    MOD = "MOD",
    MODULE = "MODULE",
    NIL = "NIL",
    OF = "OF",
    OR = "OR",
    POINTER = "POINTER",
    PROCEDURE = "PROCEDURE",
    RECORD = "RECORD",
    REPEAT = "REPEAT",
    RETURN = "RETURN",
    THEN = "THEN",
    TO = "TO",
    TRUE = "TRUE",
    TYPE = "TYPE",
    UNTIL = "UNTIL",
    VAR = "VAR",
    WHILE = "WHILE",
    WITH = "WITH",
}

struct Scanner
{
    private string input;

    Token token;

    string value;

    string comment;

    this(string input)
    {
        this.input = input;
        popFront;
    }

    bool empty()
    {
        return token == Token.eof;
    }

    Token front()
    {
        return token;
    }

    void popFront()
    {
        token = read;
    }

    private Token read()
    {
        comment = null;
        while (true)
        {
            while (input.next.isWhite)
                input.popFront;
            if (input.empty)
                return Token.eof;
            if (input.front.isAlpha)
                return readIdent;
            if (input.front.isDigit)
                return readNumber;
            switch (input.front)
            {
            case '"':
            case '\'':
                return readString;
            case '#':
                input.popFront;
                return Token.notEqual;
            case '&':
                input.popFront;
                return Token.and;
            case '(':
                input.popFront;
                if (input.next == '*')
                {
                    comment ~= readComment;
                    continue;
                }
                return Token.leftParen;
            case ')':
                input.popFront;
                return Token.rightParen;
            case '*':
                input.popFront;
                return Token.times;
            case '+':
                input.popFront;
                return Token.plus;
            case ',':
                input.popFront;
                return Token.comma;
            case '-':
                input.popFront;
                return Token.minus;
            case '.':
                input.popFront;
                if (input.next == '.')
                {
                    input.popFront;
                    return Token.upTo;
                }
                return Token.period;
            case '/':
                input.popFront;
                return Token.slash;
            case ':':
                input.popFront;
                if (input.next == '=')
                {
                    input.popFront;
                    return Token.becomes;
                }
                return Token.colon;
            case ';':
                input.popFront;
                return Token.semicolon;
            case '<':
                input.popFront;
                if (input.next == '=')
                {
                    input.popFront;
                    return Token.lessEqual;
                }
                return Token.less;
            case '=':
                input.popFront;
                return Token.equal;
            case '>':
                input.popFront;
                if (input.next == '=')
                {
                    input.popFront;
                    return Token.greaterEqual;
                }
                return Token.greater;
            case '[':
                input.popFront;
                return Token.leftBracket;
            case ']':
                input.popFront;
                return Token.rightBracket;
            case '^':
                input.popFront;
                return Token.arrow;
            case '{':
                input.popFront;
                return Token.leftBrace;
            case '}':
                input.popFront;
                return Token.rightBrace;
            case '|':
                input.popFront;
                return Token.bar;
            case '~':
                input.popFront;
                return Token.not;
            default:
                enforce(false, format("unexpected character '%s'", input.front));
            }
        }
    }

    unittest
    {
        with (Scanner("<="))
        {
            assert(token == Token.lessEqual);
            assert(input.empty);
        }
    }

    // ident  =  letter {letter | digit}.
    private Token readIdent()
    {
        import std.traits : EnumMembers;

        string copy = input;
        while (!input.empty && input.front.isAlphaNum)
            input.popFront;
        value = copy[0 .. $ - input.length];
        foreach (token; EnumMembers!Token)
            if (value == token)
                return token;
        return Token.ident;
    }

    unittest
    {
        with (Scanner("Oberon2"))
        {
            assert(token == Token.ident);
            assert(value == "Oberon2");
            assert(input.empty);
        }
    }

    unittest
    {
        with (Scanner("BEGIN"))
        {
            assert(token == Token.BEGIN);
            assert(input.empty);
        }
    }

    // number = integer | real.
    // integer = digit {digit} | digit {hexDigit} "H".
    // real = digit {digit} "." {digit} [ScaleFactor].
    // ScaleFactor = "E" ["+" | "-"] digit {digit}.
    // hexDigit = digit | "A" | "B" | "C" | "D" | "E" | "F".
    // digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
    //
    // character = digit {hexDigit} "X".
    private Token readNumber()
    {
        import std.conv : to;

        string copy = input;
        while (input.next.isHexDigit)
            input.popFront;
        if (input.next == 'X')
        {
            value = format(`\x%02x`, copy[0 .. $ - input.length].to!uint(16));
            input.popFront;
            return Token.character;
        }
        if (input.next == 'H')
        {
            value = copy[0 .. $ - input.length].to!uint(16).to!string;
            input.popFront;
            return Token.integer;
        }
        if (input.next == '.' &&
            (input.length < "..".length || input[1] != '.'))
        {
            input.popFront;
            while (input.next.isDigit)
                input.popFront;
            if (!input.empty && (input.front == 'E' || input.front == 'D'))
            {
                input.popFront;
                if (!input.empty && (input.front == '+' || input.front == '-'))
                    input.popFront;
                enforce(input.next.isDigit, "digit missing");
                while (input.next.isDigit)
                    input.popFront;
            }
            value = copy[0 .. $ - input.length];
            return Token.real_;
        }
        value = copy[0 .. $ - input.length];
        return Token.integer;
    }

    unittest
    {
        with (Scanner("42"))
        {
            assert(token == Token.integer);
            assert(value == "42");
            assert(input.empty);
        }
    }

    unittest
    {
        with (Scanner("0AH"))
        {
            assert(token == Token.integer);
            assert(value == "10");
            assert(input.empty);
        }
    }

    unittest
    {
        with (Scanner("1.E+2"))
        {
            assert(token == Token.real_);
            assert(value == "1.E+2");
            assert(input.empty);
        }
    }

    unittest
    {
        with (Scanner("0AX"))
        {
            assert(token == Token.character);
            assert(value == `\x0a`);
            assert(input.empty);
        }
    }

    // string = '"' {char} '"' | "'" {char} "'".
    private Token readString()
    {
        const delimiter = input.front;
        input.popFront;
        string copy = input;
        while (input.next != delimiter)
        {
            enforce(!input.empty, "unterminated string");
            input.popFront;
        }
        value = copy[0 .. $ - input.length];
        input.popFront;
        return Token.string_;
    }

    unittest
    {
        with (Scanner(`"Oberon-2"`))
        {
            assert(token == Token.string_);
            assert(value == "Oberon-2");
            assert(input.empty);
        }
    }

    unittest
    {
        with (Scanner(`'"'`))
        {
            assert(token == Token.string_);
            assert(value == `"`);
            assert(input.empty);
        }
    }

    private string readComment()
    {
        input.popFront;
        string copy = input;
        do
        {
            while (input.next != '*')
            {
                enforce(!input.empty, "unterminated comment");
                if (input.front == '(')
                {
                    input.popFront;
                    if (input.next == '*')
                        readComment;
                }
                else
                    input.popFront;
            }
            while (input.next == '*')
                input.popFront;
        }
        while (input.next != ')');
        input.popFront;
        return copy[0 .. $ - input.length - 2];
    }

    unittest
    {
        with (Scanner("(*(***)*)"))
        {
            assert(token == Token.eof);
            assert(comment == "(***)");
            assert(input.empty);
        }
    }
}

dchar next(string input)
{
    return input.empty ? 0 : input.front;
}
