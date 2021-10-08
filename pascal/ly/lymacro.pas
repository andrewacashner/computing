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

uses SysUtils, Classes, Generics.Collections;

type
  TMacroDict     = specialize TDictionary<String, String>;
  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ Find a macro in the form `key = value\n` and return a pair with the key and
  value }
function ExtractMacro(Source: String): TMacroKeyValue;
const
  Delim: String = '=';
var
  KeyLabel, Value: String;
  EndKeyLabel, StartValueText: Integer;
  Macro: TMacroKeyValue;
  Index: Integer;
begin
  Index    := LastDelimiter(Delim, Source);
  KeyLabel := Trim(LeftStr(Source, Index - 1));
  Value    := Trim(RightStr(Source, Length(Source) - Index));

  try
    Macro := TMacroKeyValue.Create(KeyLabel, Value);
  finally 
    result := Macro;
  end;
end;

function MacroExists(Pair: TMacroKeyValue): Boolean;
begin
  result := not ((Pair.Key = '') or (Pair.Value = ''))
end;

{ MAIN }
var
  Macros:     TMacroDict;
  MacroPair:  TMacroKeyValue;
  InputText: Array of String = 
    ( 'MusicSoprano = { c''4 d''4 es''4 }'
    , 'This is not a macro'
    , 'LyricsSoprano = \lyricmode { ly -- ric text }'
    );
  ThisString: String;

begin
  Macros := TMacroDict.Create();
  try
    for ThisString in InputText do
    begin
      MacroPair := ExtractMacro(ThisString);
      if MacroExists(MacroPair) then
      begin
        Macros.Add(MacroPair);
      end;
    end;

    for MacroPair in Macros do
    begin
      WriteLn('key: ' + MacroPair.Key + ', value: ' + MacroPair.Value);
    end;

  finally
    FreeAndNil(Macros);
  end;
end.

