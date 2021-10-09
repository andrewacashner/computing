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

uses SysUtils, StrUtils, Classes, Generics.Collections;

type
  TMacroDict     = specialize TDictionary<String, String>;
  TMacroKeyValue = TMacroDict.TDictionaryPair;

function MatchBraces(Source: String): String;
var 
  Found: Boolean = False;
  Test: String;
  StartIndex: Integer = 0;
  EndIndex:   Integer = 0;
  BraceLevel: Integer = 0;
begin
  Test := Source.Substring(StartIndex);
  // WriteLn('Testing substring ' + Test);
  while Test.Contains('{') do
  begin 
    if Test.IndexOf('{') < Test.IndexOf('}') then
    begin
      // WriteLn('Found a { before a }');
      StartIndex := Test.IndexOf('{') + 1;
      // WriteLn('Next { is before index ' + IntToStr(StartIndex));
      Inc(BraceLevel);
      Test := Test.Substring(StartIndex);
      Found := False;
    end;
    while Test.Contains('}') and (BraceLevel > 0) do
    begin
      // WriteLn('found }, bracelevel = ' + IntToStr(BraceLevel));
      EndIndex := Test.IndexOf('}');
      Dec(BraceLevel);
      Test := Test.Substring(StartIndex, EndIndex);
      Found := True;
    end;
  end;
  if Found and (BraceLevel = 0) then
  begin
    // WriteLn('found an expression and bracelevel = 0');
    result := Source.Substring(StartIndex, EndIndex);
  end
  else
  begin
    result := '';
  end;
end;

function FindExpression(Source: String): String;
begin
  if Source.StartsWith('\lyricmode') then
  begin
    // WriteLn('Found \lyricmode');
    result := MatchBraces(Source.Substring(Source.IndexOf('{') - 1));
  end
  else
    result := MatchBraces(Source);
end;

{ Find a macro in the form `key = value\n` and return a pair with the key and
  value. A macro definition label must start at the beginning of the line and
  be a single string of alphabetic characters.  }
function ExtractMacro(Source: String): TMacroKeyValue;
const
  Delim: String = '=';
var
  KeyLabel, Value: String;
  Macro: TMacroKeyValue;
  Found: Boolean;
begin
  try
    if Source.Contains(Delim) then
    begin
      KeyLabel := Trim(Source.Substring(0, Source.IndexOf(Delim) - 1));
      if KeyLabel = ExtractWord(1, Source, StdWordDelims) then
      begin
        Value := Trim(Source.Substring(Source.IndexOf(Delim) + 1));
        // WriteLn('Trying to find value expression in string: ' + Value);
        Value := FindExpression(Value);
        // WriteLn('found macro, key: ' + KeyLabel + ', value: ' + Value);
        Found := True;
      end;
    end;
   
    if Found then
      Macro := TMacroKeyValue.Create(KeyLabel, Value)
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
    , 'MusicBass = { c8 d8 e8 f8 g2 g,2'
    , '  c4 e4 f4 g4 c1 }'
    , ''
    , 'MusicAc = { c1 c1 \MusicBass }'
    , '\new Staff = "Basses"'
    , '  <<'
    , '    \new Voice = "B1" { \voiceOne \MusicBass }'
    , '    \new Voice = "B2" { \voiceTwo \MusicAc }'
    , '  >>'
    );

var
  Macros:     TMacroDict;
  MacroPair:  TMacroKeyValue;
  InputStr, ThisStr, TestStr: String;
  OutputStr: String;
  Index: Integer;
  ReplaceStart, ReplaceEnd: Integer;
begin
  Macros := TMacroDict.Create();
  try
    InputStr := '';
    for ThisStr in InputText do
      InputStr := InputStr + ThisStr + LineEnding;

    TestStr := InputStr;
    // WriteLn('Looking for macro definitions in input text: ' + TestStr);
    while not TestStr.IsEmpty do
    begin
      MacroPair := ExtractMacro(TestStr);
      if MacroExists(MacroPair) then
      begin
        // WriteLn('Found and confirmed macro');
        Macros.AddOrSetValue(MacroPair.Key, MacroPair.Value);
        TestStr := TestStr.Substring(TestStr.IndexOf(MacroPair.Value));
      end;

      Index := TestStr.IndexOf(LineEnding);
      // WriteLn('Looking for next newline delimiter in string: ' + TestStr);
      // WriteLn('Next delimiter is at index ' + IntToStr(Index));
      if Index = -1 then
        break
      else
        TestStr := TestStr.Substring(Index + 1)
    end;

    { Automate multiple passes, detect when all macros are substituted }
    OutputStr := InputStr;
    OutputStr := FindReplaceMacros(OutputStr, Macros);
    OutputStr := FindReplaceMacros(OutputStr, Macros);

    { how to trim out macro definitions?
      one issue here is the macro names (\Bass and \BassII are not treated as separate names)

    for MacroPair in Macros do
    begin
      ThisStr := OutputStr.Substring(OutputStr.IndexOf(MacroPair.Key));
      ThisStr := OutputStr.Substring(OutputStr.IndexOf(MacroPair.Value));
      ReplaceStart := OutputStr.IndexOf(MacroPair.Key);
      ReplaceEnd := OutputStr.IndexOf(MacroPair.Value) + length(MacroPair.Value) + 1;
      OutputStr := OutputStr.Substring(0, ReplaceStart) + 
                    OutputStr.Substring(ReplaceEnd);
    end;
    }

  finally
    WriteLn(OutputStr);
    FreeAndNil(Macros);
  end;
end.

