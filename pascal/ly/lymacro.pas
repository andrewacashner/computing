{ 
  `lymacro`

  Andrew Cashner, 2021/10/07
 
  A basic macro-substitution program targeting Lilypond code.  
  Lilypond allows users to define macros like this:
  
  ~~~~
  MusicSoprano = { c''4 d''4 ees''4 }
  LyricsSoprano = \lyricmode { ly -- ric text }
  ~~~~
  
  Then they can call them like `\MusicSoprano` or `\LyricsSoprano`.
  Instead of actually processing the arguments like Lilypond does, we just
  want to do simple textual substitution of the argument for the macro label.
}

{$mode objfpc}{$H+}{$J-}
program lymacro(input, output);

uses SysUtils, Classes, Generics.Collections, Strings;

function ExtractMacro(S: String): TMacroKeyValue;
const
  Delim: String := '='
var
  KeyLabel, Value: String;
  EndKeyLabel, StartValueText: Integer;
  Macro: TMacroKeyValue;
begin
  Value := StrRScan(S, Delim));
  KeyLabel := LeftStr(S,LastDelimiter(Delim, S) - 1);
  if Value <> nil then
    Value := Trim(Value);
  if KeyLabel <> nil then
    KeyLabel := Trim(KeyLabel);

  try
    Macro := TMacroKeyValue.Create(KeyLabel, Value);
  finally 
    result := Macro;
  end;
end;

type
  TMacroKey      = specialize TKey<String>;
  TMacroValue    = specialize TValue<String>;
  TMacroKeyValue = specialize TPair<TMacroKey, TMacroValue>;
  TMacroDict     = specialize TDictionary<TMacroKeyValue>;

{ MAIN }
var
  Macros: TMacroDict;
  InputText: String = 'MusicSoprano = { c''4 d''4 es''4 }';
  V: String;
begin
  Macros := TMacroDict.Create();
  try
    Macros.Add('MusicSoprano', '{ c''4 d''4 es''4 }');
    Macros.Add('LyricsSoprano', '\lyricmode { ly -- ric text }');
    Macros.TryGetValue('MusicSoprano', V);
  finally
    WriteLn(V);
    FreeAndNil(macros);
  end;
end.

