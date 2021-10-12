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
program lymacro(input, output, stderr);

uses SysUtils, StrUtils, Classes, Generics.Collections;

procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
end;

type
  TMacroDict     = specialize TDictionary<String, String>;
  TMacroKeyValue = TMacroDict.TDictionaryPair;
  
  TMacroOutline = class 
  private
    FKeyStart, FKeyEnd, FValueStart, FValueEnd: Integer;
    FValid: Boolean;
  public
    function KeyLength: Integer;
    function ValueLength: Integer;
    function MacroLength: Integer;
    procedure Clear;
  end;

function TMacroOutline.KeyLength: Integer;
begin
  result := FKeyEnd - FKeyStart;
end;

function TMacroOutline.ValueLength: Integer;
begin
  result := FValueEnd - FValueStart;
end;

function TMacroOutline.MacroLength: Integer;
begin
  result := FValueEnd - FKeyStart;
end;

procedure TMacroOutline.Clear;
begin
  FKeyStart := 0;
  FKeyEnd := 0;
  FValueStart := 0;
  FValueEnd := 0;
  FValid := False;
end;

{ Find a macro definition and return a structure with the four indices for the
start and end of the key and of the value; if none found, return the object
with `FValid` marked False. 

- The KEY must be a single alphabetic word, starting at the beginning fo the line.
- The DELIMITER is '=', optionally surrounded by whitespace.
- The VALUE can be any of the following:
    - A music expression enclosed in curly braces: `key = { music }`
    - A macro command with no arguments: `key = \macro`
    - A macro command with arguments: `key = \lyricmode { lyrics }` 

NB we are not accepting `MarkupMacro = \markup "string"` as a valid definition, only `MarkupMacro = \markup { "string" }`.
}
function MarkMacro(Outline: TMacroOutline; Source:String): TMacroOutline;
type
  TReadMode = (rkNormal, rkCommand, rkBraceArgument);
var
  C: String;
  Key, Value: String;

  SplitPoint, CommandStart, CommandEnd, 
  ArgumentStart, ArgumentEnd, BraceLevel: Integer;

  CommandFound, ArgumentFound: Boolean;
  ReadMode: TReadMode;

  SIndex: Integer;
begin
  Outline.Clear;
  if Source.Contains('=') then
  begin
    SplitPoint := Source.IndexOf('=');

    { The key must be the first word in the line }
    Key := Source.Substring(0, SplitPoint - 1);
    DebugLn('testing possible key: ' + Key.Substring(0, 20));

    if Key = ExtractWord(1, Source, StdWordDelims) then
    begin
      Outline.FKeyStart := 0;
      Outline.FKeyEnd := SplitPoint;
      DebugLn('found key: start ' + IntToStr(Outline.FKeyStart) 
      + ', end: ' + IntToStr(Outline.FKeyEnd));

      { Find expression in matched curly braces }

      ReadMode := rkNormal;
      BraceLevel := 0;
      CommandFound := False;
      ArgumentFound := False;

      SIndex := SplitPoint + 2;
      while SIndex < Length(Source) do
      begin
        C := Source[SIndex];
        DebugLn('test source char: ' + C);
        case C of
          '\':
          begin
            if ReadMode = rkNormal then
            begin
              CommandStart := SIndex - 1;
              DebugLn('Found \ at index ' + IntToStr(CommandStart));
              ReadMode := rkCommand;
              Value := ExtractWord(1, Source.Substring(CommandStart), StdWordDelims);
              DebugLn('Found command \' + Value);
              CommandEnd := CommandStart + Length(Value) + 1;
              CommandFound := True;

              { search for argument immediately after command (with optional
              whitespace between) }
              SIndex := CommandEnd + 1;
              DebugLn('checking for arg in substring starting: ' + Source.Substring(SIndex, 20));
              if Source.Substring(SIndex).TrimLeft.StartsWith('{') then
              begin
                DebugLn('looking for argument after command');
              end
              else
              begin
                ArgumentFound := False;
                DebugLn('no argument found after command; ending the search');
                break;
              end;
            end; { if ReadMode }
          end;

          '{': 
          begin
            DebugLn('Found { at index ' + IntToStr(SIndex));
            if (ReadMode = rkNormal) and (BraceLevel = 0) then
            begin
              ArgumentStart := SIndex - 1; { include opening bracket in value string }
            end;
            Inc(BraceLevel);
            DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
            ReadMode := rkBraceArgument;
          end;

          '}': 
          begin
            if ReadMode = rkBraceArgument then
            begin
              DebugLn('Found } at index ' + IntToStr(SIndex));
              Dec(BraceLevel);
              DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
              if BraceLevel = 0 then
              begin
                ArgumentEnd := SIndex; { include close bracket }
                ArgumentFound := True;
                DebugLn('Found an expression, ending the search');
                break;
              end;
            end;
          end; 
        end; { case }
        Inc(SIndex);
      end; { for }

      if CommandFound then
      begin { `\command { argument }` }
        if ArgumentFound then
        begin
          Outline.FValueStart := CommandStart;
          Outline.FValueEnd   := ArgumentEnd;
          Outline.FValid      := True;
        end
        else { `\command` }
        begin
          Outline.FValueStart := CommandStart;
          Outline.FValueEnd   := CommandEnd;
          Outline.FValid      := True;
        end
      end
      else { `{ argument }` }
      begin
        if ArgumentFound then
        begin
          Outline.FValueStart := ArgumentStart;
          Outline.FValueEnd   := ArgumentEnd;
          Outline.FValid      := True;
        end;
      end;
       
      {$ifdef DEBUG}
      if CommandFound or ArgumentFound then
      begin
        DebugLn('found value: start ' + IntToStr(Outline.FValueStart) 
        + ', end: ' + IntToStr(Outline.FValueEnd));
      end;
      {$endif}
    end;
  end;

  {$ifdef DEBUG}
  if Outline.FValid = False then
  begin 
    DebugLn('no macro definition found');
  end;
  {$endif}

  result := Outline;
end;

{ Find a command starting with backslash like `\Music`, look up the key in
dictionary and if found, replace it with the corresponding value; if nothing
is found, just leave the text alone. 
Expand any macros in the stored values before expanding them in the source
text. } 
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;
var
  Macro: TMacroKeyValue;

function Replace(S: String; Dict: TMacroDict): String;
begin
  for Macro in Dict do
    S := S.Replace(Macro.Key, Macro.Value, [rfReplaceAll]);
  { TODO need to sort keys, otherwise if you define `\LyricsI` and \LyricsII`
  then `\LyricsI` will be expanded inside the string `\LyricsII` }
  result := S;
end;

begin
  for Macro in Dict do
  begin
    { TODO ? first expand any macros stored in the dictionary values }
    // Dict.AddOrSetValue(Macro.Key, Replace(Macro.Value, Dict));
    Source := Replace(Source, Dict);
  end;
  result := Source;
end;

{ Remove the first instance of a substring from a string }
function CensorString(Source: String; Block: String): String;
var
  CutFrom, CutTo: Integer;
begin
  CutFrom := Source.IndexOf(Block);

  if CutFrom = -1 then
    result := Source { no substring found }
  else
  begin
    CutTo := CutFrom + Length(Block) + 1;
    result := Source.Substring(0, CutFrom) + Source.Substring(CutTo);
  end;
end;

function StringDropBefore(InputStr: String; Delim: String): String;
var
  BreakPoint: Integer;
begin
  if InputStr.Contains(Delim) then
  begin
    BreakPoint := InputStr.IndexOf(Delim);
    InputStr := InputStr.Substring(BreakPoint, Length(InputStr) - BreakPoint);
  end;
  result := InputStr;
end;

function StringDropAfter(InputStr: String; Delim: String): String;
begin
  if InputStr.Contains(Delim) then
    InputStr := InputStr.Substring(0, InputStr.IndexOf(Delim));
  result := InputStr;
end;



function RemoveComments(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].StartsWith('%') then
      InputLines.Delete(I)
    else
      InputLines[I] := StringDropAfter(InputLines[I], '%');
  end;
  result := InputLines;
end;

function RemoveBlankLines(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].Trim.IsEmpty then
      InputLines.Delete(I);
  end;
  result := InputLines;
end;

{ Split a string at newlines to make a `TStringList` }
function Lines(InputStr: String; OutputList: TStringList): TStringList;
begin
  OutputList.Clear;
  OutputList.Delimiter := LineEnding;
  OutputList.StrictDelimiter := True;
  OutputList.DelimitedText := InputStr;
  result := OutputList;
end;

{ MAIN }
var
  InputText, OutputText: TStringList;
  Macros: TMacroDict;
  Outline: TMacroOutline;
  FileName, ThisStr, BufferStr, CutStr, Key, Value: String;
  NewStart: Integer;
  {$ifdef DEBUG}
  MacroPair: TMacroKeyValue;
  {$endif}
begin
  InputText := TStringList.Create();
  OutputText := TStringList.Create();
  Outline := TMacroOutline.Create();
  Macros := TMacroDict.Create();

  try
    { Process input file }
    if ParamCount <> 1 then
    begin
      WriteLn('Usage: lymacro INFILE.ly');
      exit;
    end
    else
    begin
      FileName := ParamStr(1);
      DebugLn('Loading input file ' + FileName);
    end;

    InputText.LoadFromFile(FileName);

    InputText := RemoveComments(InputText);

    { Look for macro definitions; store them in macro dictionary and delete
    them from the output text. If none found in this line, go to the next.  }
    BufferStr := InputText.Text;
    ThisStr := BufferStr;
    while not ThisStr.IsEmpty do
    begin
      Outline := MarkMacro(Outline, ThisStr);
      if Outline.FValid then
      begin
        Key   := '\' + ThisStr.Substring(Outline.FKeyStart, Outline.KeyLength).Trim;
        Value := ThisStr.Substring(Outline.FValueStart, Outline.ValueLength).Trim;

        Macros.AddOrSetValue(Key, Value);
        DebugLn('SUCCESS: added macro key: ' + Key + ', value: ' + Value);

        { Cut out the macro definition from the output text. }
        CutStr := ThisStr.Substring(Outline.FKeyStart, Outline.MacroLength);
        BufferStr := CensorString(BufferStr, CutStr);

        { Move ahead to end of macro definition. }
        NewStart := Outline.FValueEnd;
        ThisStr := ThisStr.Substring(NewStart);
      end
      else
      begin
        { If no macro found on this line, try the next. }
        DebugLn('did not find a macro on this line');
        NewStart := ThisStr.IndexOf(LineEnding) + 1;
        ThisStr := ThisStr.Substring(NewStart);
      end;
    end;

    {$ifdef DEBUG}
    for MacroPair in Macros do
      DebugLn('MACRO: key = ' + MacroPair.Key + ', value = ' + MacroPair.Value);
    {$endif}

    BufferStr := FindReplaceMacros(BufferStr, Macros);
    OutputText := RemoveBlankLines(Lines(BufferStr, OutputText));
    WriteLn(OutputText.Text);

  finally
    FreeAndNil(Macros);
    FreeAndNil(Outline);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

