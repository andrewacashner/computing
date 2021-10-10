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
with `FValid` marked False. }
function MarkMacro(Outline: TMacroOutline; Source:String): TMacroOutline;
var
  Key: String;
  SplitPoint, I, StartI, EndI: Integer;
  BraceLevel: Integer = 0;
  Found: Boolean = False;
begin
  Outline.Clear;
  if Source.Contains('=') then
  begin
    SplitPoint := Source.IndexOf('=');
    Key := Source.Substring(0, SplitPoint - 1);

    { The key must be the first word in the line }
    if Key = ExtractWord(1, Source, StdWordDelims) then
    begin
      Outline.FKeyStart := 0;
      Outline.FKeyEnd := SplitPoint - 1;

      { Find expression in matched curly braces }
      for I := SplitPoint + 1 to Length(Source) do
      begin
        case Source[I] of
          '{': 
            begin
              DebugLn('Found { at index ' + IntToStr(I));
              if BraceLevel = 0 then
              begin
                StartI := I + 1;
              end;
              Inc(BraceLevel);
              DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
              Found := False;
            end;
          '}': 
            begin
              DebugLn('Found } at index ' + IntToStr(I));
              EndI := I - 1;
              Dec(BraceLevel);
              DebugLn('Going to bracelevel ' + IntToStr(BraceLevel));
              Found := True;
              if BraceLevel = 0 then
              begin
                DebugLn('Found an expression, ending the search');
                break;
              end;
            end;
        end;
      end;
      if Found and (BraceLevel = 0) then
      begin
        Outline.FValueStart := StartI;
        Outline.FValueEnd   := EndI;
        Outline.FValid      := True;
        DebugLn('found value: start ' + IntToStr(Outline.FValueStart) 
                  + ', end: ' + IntToStr(Outline.FValueEnd));
      end
      else
      begin
        Outline.FValid := False;
      end;
    end;
  end;
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

{ MAIN }
var
  InputText: TStringList;
  Macros: TMacroDict;
  Outline: TMacroOutline;
  FileName, ThisStr, OutputStr, CutStr, Key, Value: String;
  NewStart: Integer;
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

        { The value can either be in the form `{ music }` or `\lyricmode {
        lyrics }`. We look for `\lyricmode` first, then we must find a
        matching brace group. }
        Value := TrimLeft(ThisStr.Substring(ThisStr.IndexOf('=') + 1));
        DebugLn('Looking for \lyricmode in: ' + Value);
        if Value.StartsWith('\lyricmode') then
        begin
          DebugLn('Found \lyricmode');
          Value := '\lyricmode ';
        end
        else
        begin
          Value := '';
        end;
        Value := Value + Trim(ThisStr.Substring(Outline.FValueStart, Outline.ValueLength));

        Macros.AddOrSetValue(Key, Value);
        DebugLn('added macro key: ' + Key + ', value: ' + Value);

        { Keep looking after end of macro-definition expression. }
        NewStart := Outline.FValueEnd + 1;

        { Cut out the macro definition from the output text. }
        CutStr := ThisStr.Substring(Outline.FKeyStart, Outline.MacroLength);
        OutputStr := CensorString(OutputStr, CutStr);
      end
      else
      begin
        { If no macro found on this line, try the next. }
        DebugLn('did not find a macro');
        NewStart := ThisStr.IndexOf(LineEnding) + 1;
      end;
      ThisStr := ThisStr.Substring(NewStart);
    end;
   
    OutputStr := FindReplaceMacros(OutputStr, Macros);
    WriteLn(OutputStr);

  finally
    FreeAndNil(Macros);
    FreeAndNil(Outline);
    FreeAndNil(InputText);
  end;
end.

