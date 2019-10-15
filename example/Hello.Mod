MODULE Hello;
  IMPORT Texts, Oberon;
  VAR W: Texts.Writer;

  PROCEDURE World*;
  BEGIN Texts.WriteString(W, "hello, world");
    Texts.WriteLn(W);
    Texts.WriteString(W, "where -8 divided by 3 is ");
    Texts.WriteInt(W, (-8) DIV 3);
    Texts.WriteLn(W);
    Texts.Append(Oberon.Log, W.buf)
  END World;

BEGIN Texts.OpenWriter(W)
END Hello.
