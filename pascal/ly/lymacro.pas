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
function FindReplaceMacro(Source: String; Dict: TMacroDict): String;
var
  AfterSlash: String;
  Command, Expansion: String;
  StartIndex, EndIndex: Integer;
begin
  if not Source.Contains('\') then
    result := Source
  else
  begin
    while Source.Contains('\') do
    begin
      StartIndex := Source.IndexOf('\');
      AfterSlash := Source.Substring(StartIndex + 1);
      EndIndex   := AfterSlash.IndexOf(' ');
      Command    := AfterSlash.Substring(0, EndIndex);

      if Dict.TryGetValue(Command, Expansion) then
        result := Source.Replace('\' + Command, Expansion)
      else
        result := Source;

      Source := AfterSlash;
    end;
  end;
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
          Macros.AddOrSetValue(MacroPair.Key, MacroPair.Value);
    end;

    for MacroPair in Macros do
    begin
      WriteLn('key: ' + MacroPair.Key + ', value: ' + MacroPair.Value);
    end;

    for ThisString in InputText do
    begin
      OutputText.Add(FindReplaceMacro(ThisString, Macros));
    end;


  finally
    WriteLn(OutputText.Text);
    FreeAndNil(Macros);
    FreeAndNil(OutputText);
  end;
end.

