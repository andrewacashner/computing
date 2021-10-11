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
  Key, Value, TestStr: String;

  SplitPoint, I, CommandStart, CommandEnd, 
  ArgumentStart, ArgumentEnd, BraceLevel: Integer;

  CommandFound, ArgumentFound: Boolean;
  ReadMode: TReadMode;

begin
  Outline.Clear;
  if Source.Contains('=') then
  begin
    SplitPoint := Source.IndexOf('=');

    { The key must be the first word in the line }
    Key := Source.Substring(0, SplitPoint - 1);
    DebugLn('testing possible key: ' + Key);

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

      TestStr := Source.Substring(SplitPoint + 2);
      while not TestStr.IsEmpty do
      begin
        for C in TestStr do
        begin
          DebugLn('test source char: ' + C);
          case C of
          '%': 
          begin
            { START TODO this doesn't work }
            { skip to end of line after comment char }
            I := TestStr.IndexOf(LineEnding) + 1;
            TestStr := TestStr.Substring(I);
          end;

          '\':
          begin
            if ReadMode = rkNormal then
            begin
              CommandStart := Source.IndexOf(C);
              DebugLn('Found \ at index ' + IntToStr(CommandStart));
              ReadMode := rkCommand;
              Value := ExtractWord(1, Source.Substring(CommandStart), StdWordDelims);
              DebugLn('Found command \' + Value);
              CommandEnd := CommandStart + Length(Value) + 1;
              CommandFound := True;

              { search for argument immediately after command (with optional
              whitespace between) }
              I := CommandEnd + 1;
              DebugLn('checking for arg in substring: ' + Source.Substring(I));
              if TrimLeft(Source.Substring(I)).StartsWith('{') then
              begin
                DebugLn('looking for argument after command');
                TestStr := Source.Substring(I);
              end
              else
              begin
                ArgumentFound := False;
                TestStr := ''; { end the loop }
                DebugLn('no argument found after command; ending the search');
                break;
              end;
            end; { if ReadMode }
          end;

          '{': 
          begin
            I := Source.IndexOf(C);
            DebugLn('Found { at index ' + IntToStr(I));
            if (ReadMode = rkNormal) and (BraceLevel = 0) then
            begin
              ArgumentStart := I - 1; { include opening bracket in value string }
            end;
            Inc(BraceLevel);
            DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
            ReadMode := rkBraceArgument;
            TestStr := TestStr.Substring(I + 1);
            continue;
          end;

          '}': 
          begin
            if ReadMode = rkBraceArgument then
            begin
              I := Source.IndexOf(C);
              DebugLn('Found } at index ' + IntToStr(I));
              ArgumentEnd := I + 1; { include close bracket }
            end;
            Dec(BraceLevel);
            DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
            if BraceLevel = 0 then
            begin
              ArgumentFound := True;
              DebugLn('Found an expression, ending the search');
              break;
            end
            else
            begin
              TestStr := TestStr.Substring(I + 1);
              continue
            end;
          end; 

          else
          begin
            TestStr := TestStr.Substring(I + 1);
          end;
          end; { case }
        end; { for }
      end; { while }

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
    S := S.Replace('\' + Macro.Key, Macro.Value, [rfReplaceAll]);
  result := S;
end;

begin
  for Macro in Dict do
  begin
    { first expand any macros stored in the dictionary values }
    Dict.AddOrSetValue(Macro.Key, Replace(Macro.Value, Dict));
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

function RemoveComments(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].StartsWith('%') then
    begin
      InputLines.Delete(I);
    end;
  end;
  result := InputLines;
end;

{ MAIN }
var
  InputText: TStringList;
  Macros: TMacroDict;
  Outline: TMacroOutline;
  FileName, ThisStr, OutputStr, CutStr, Key, Value: String;
  NewStart: Integer;
  {$ifdef DEBUG}
  MacroPair: TMacroKeyValue;
  {$endif}
begin
  InputText := TStringList.Create();
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
    OutputStr := InputText.Text;
    ThisStr := OutputStr;
    while not ThisStr.IsEmpty do
    begin
      Outline := MarkMacro(Outline, ThisStr);
      if Outline.FValid then
      begin
        Key   := Trim(ThisStr.Substring(Outline.FKeyStart, Outline.KeyLength));
        Value := Trim(ThisStr.Substring(Outline.FValueStart, Outline.ValueLength));

        Macros.AddOrSetValue(Key, Value);
        DebugLn('SUCCESS: added macro key: ' + Key + ', value: ' + Value);

        { Keep looking after end of macro-definition expression. }
        NewStart := Outline.FValueEnd + 1;

        { Cut out the macro definition from the output text. }
        CutStr := ThisStr.Substring(Outline.FKeyStart, Outline.MacroLength);
        OutputStr := CensorString(OutputStr, CutStr);
      end
      else
      begin
        { If no macro found on this line, try the next. }
        DebugLn('did not find a macro on this line');
        NewStart := ThisStr.IndexOf(LineEnding) + 1;
      end;
      ThisStr := ThisStr.Substring(NewStart);
    end;

    {$ifdef DEBUG}
    for MacroPair in Macros do
      WriteLn('MACRO: key = ' + MacroPair.Key + ', value = ' + MacroPair.Value);
    {$endif}

    OutputStr := FindReplaceMacros(OutputStr, Macros);
    WriteLn(OutputStr);

  finally
    FreeAndNil(Macros);
    FreeAndNil(Outline);
    FreeAndNil(InputText);
  end;
end.

