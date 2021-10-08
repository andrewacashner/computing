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

{ Return the portion of a string that follows a delimiter string }
function TakeAfterDelimiter(S: String; Delim: String): String;
begin
  result := RightStr(S, (Length(S) - LastDelimiter(Delim, S)));
end;

{ Find a macro in the form `key = value\n` and return a pair with the key and
  value }
function ExtractMacro(Source: String): TMacroKeyValue;
const
  Delim: String = '=';
var
  KeyLabel, Value: String;
  Macro: TMacroKeyValue;
  Index: Integer;
begin
  Index    := LastDelimiter(Delim, Source);
  KeyLabel := Trim(LeftStr(Source, Index - 1));
  Value    := Trim(TakeAfterDelimiter(Source, Delim));

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

{ Find a command starting with backslash like `\Music`, look up the key in
dictionary and if found, replace it with the corresponding value; if nothing
is found, just leave the text alone. }
function FindCallCommand(Source: String; Dict: TMacroDict): String;
var
  Delim: String = '\';
  Command, Expansion: String;
begin
  Command := Trim(TakeAfterDelimiter(Source, Delim));
  if Dict.TryGetValue(Command, Expansion) then
  begin
    result := StringReplace(Source, '\' + Command, Expansion, [rfReplaceAll]);
  end
  else
  begin
    result := Source;
  end;
end;

{ MAIN }
var
  Macros:     TMacroDict;
  MacroPair:  TMacroKeyValue;
  InputText: Array of String = 
    ( 'MusicSoprano = { c''4 d''4 es''4 }'
    , 'This is not a macro'
    , 'LyricsSoprano = \lyricmode { ly -- ric text }'
    , '\new Voice = "S" { \MusicSoprano }'
    , '\new Lyrics \lyricsto "S" { \LyricsSoprano }'
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

    for ThisString in InputText do
    begin
      WriteLn(FindCallCommand(ThisString, Macros));
    end;


  finally
    FreeAndNil(Macros);
  end;
end.

