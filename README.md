# Oberon to D

This tool translates source code from [Oberon] to [D].
The translation is purely syntactic.
In general, rework is required to run the translated code.
Also, error handling is only rudimentary.

## Usage

Use [dub] to build the tool:

    dub build

Run the tool, for example, on [Hello.Mod]:

    bin/oberon2d example/Hello.Mod

Save the translated code.
Consider prettifying the code with [dfmt]:

    bin/oberon2d example/Hello.Mod > hello.d
    bin/oberon2d example/Hello.Mod | dfmt > hello.d

A separate [main] function must be provided to execute Oberon *commands*.
The library code must be rewritten so that it adapts to D's standard library.

Build and run the example program:

    dmd hello.d example/main.d include/*.d
    ./hello

The example shows, by the way, that Oberon uses *floored* division while D uses the more common *truncated* division.
(For details, see [Division and Modulus for Computer Scientists].)

[Hello.Mod]: example/Hello.Mod
[main]: example/main.d

[Oberon]: https://people.inf.ethz.ch/wirth/Oberon/
[D]: https://dlang.org
[dub]: https://code.dlang.org
[dfmt]: https://github.com/dlang-community/dfmt
[Division and Modulus for Computer Scientists]: https://www.microsoft.com/en-us/research/publication/division-and-modulus-for-computer-scientists/
