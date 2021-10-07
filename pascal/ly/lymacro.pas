{ lymacro
  Andrew Cashner, 2021/10/07
  A basic macro-substitution program targeting Lilypond code.  
}

{$mode objfpc}{$H+}{$J-}
program lymacro(input, output);

uses SysUtils, Classes, Generics.Collections;

type
  TMacroDict = specialize TDictionary<String, String>;

  { MAIN }
var
  Macros: TMacroDict;
  V: String;
begin
  Macros := TMacroDict.Create();
  Macros.Add('MusicSoprano', '{ c''4 d''4 es''4 }');
  Macros.Add('LyricsSoprano', '\lyricmode { ly -- ric text }');
  Macros.TryGetValue('MusicSoprano', V);
  WriteLn(V);
  FreeAndNil(macros);
end.

