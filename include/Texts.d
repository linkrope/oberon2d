module Texts;

import std.stdio;

struct Writer
{
    string buf;
}

void Append(File file, string text)
{
    file.write(text);
}

void OpenWriter(Writer)
{
    // do nothing
}

void WriteLn(ref Writer writer)
{
    WriteString(writer, "\n");
}

void WriteInt(ref Writer writer, long value)
{
    import std.conv : to;

    WriteString(writer, value.to!string);
}

void WriteString(ref Writer writer, string text)
{
    writer.buf ~= text;
}
