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
  value. A macro definition label must start at the beginning of the line and
  be a single string of alphabetic characters.  }
function ExtractMacro(Source: String): TMacroKeyValue;
const
  Delim: String = '=';
var
  KeyValueStrings: TStringArray;
  KeyLabel, Value: String;
  Macro: TMacroKeyValue;
begin
  KeyValueStrings := Source.Split(Delim);
  try
    if length(KeyValueStrings) = 2 then
    begin
      KeyLabel := Trim(KeyValueStrings[0]);
      Value    := Trim(KeyValueStrings[1]);
      { If the key still has a space in it, it is not a valid key }
      if KeyLabel.Contains(' ') then
        Macro := TMacroKeyValue.Create('', '')
      else
        Macro := TMacroKeyValue.Create(KeyLabel, Value);
    end
    else
      Macro := TMacroKeyValue.Create('', '');
  finally 
    result := Macro;
  end;
end;

function MacroExists(Pair: TMacroKeyValue): Boolean;
begin
  result := not (Pair.Key.IsEmpty or Pair.Value.IsEmpty)
end;

{ Find a command starting with backslash like `\Music`, look up the key in
dictionary and if found, replace it with the corresponding value; if nothing
is found, just leave the text alone. }
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;
var
  Macro: TMacroKeyValue;
begin
  for Macro in Dict do
    Source := Source.Replace('\' + Macro.Key, Macro.Value, [rfReplaceAll]);
  result := Source;
end;

{ MAIN }
const
  InputText: Array of String = 
    ( 'MusicSoprano = { c''4 d''4 es''4 }'
    , 'This is not a macro'
    , 'LyricsSoprano = \lyricmode { ly -- ric text }'
    , '\new Voice = "S" { \MusicSoprano }'
    , '\new Lyrics \lyricsto "S" { \LyricsSoprano }'
    , '{ \MusicSoprano }'
    );

var
  Macros:     TMacroDict;
  MacroPair:  TMacroKeyValue;
  ThisString: String;
  OutputText: TStringList;
begin
  Macros := TMacroDict.Create();
  OutputText := TStringList.Create();
  try
    for ThisString in InputText do
    begin
      MacroPair := ExtractMacro(ThisString);
      if MacroExists(MacroPair) and ThisString.StartsWith(MacroPair.Key) then
          Macros.AddOrSetValue(MacroPair.Key, MacroPair.Value)
      else
        OutputText.Add(ThisString); { only add lines that aren't macro definitions }
    end;

    OutputText.Text := FindReplaceMacros(OutputText.Text, Macros);

  finally
    WriteLn(OutputText.Text);
    FreeAndNil(Macros);
    FreeAndNil(OutputText);
  end;
end.

