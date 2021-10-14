{ # `ly2mei`

  Andrew Cashner

  A converter from Lilypond to MEI-XML, specifying a strict subset of
  Lilypond.

  # Changelog

  - 2021/10/07: Began
  - 2021/10/12: Macro-substitution program complete
  - 2021/10/13: Header conversion program complete
}
{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}
program ly2mei(input, output, stderr);

uses SysUtils, Classes;

{ # Utilities }

{ `DebugLn`

  Write notes to standard error if compiled with `-dDEBUG`
}
procedure DebugLn(Msg: String);
begin
  {$ifdef DEBUG}
  WriteLn(stderr, '> ' + Msg);
  {$endif}
end;

{ # String functions }

{ `StringDropAfter`
  
  Return the portion of a string before a given delimiter.
}
function StringDropAfter(InputStr: String; Delim: String): String;
begin
  if InputStr.Contains(Delim) then
    InputStr := InputStr.Substring(0, InputStr.IndexOf(Delim));
  result := InputStr;
end;


{ # `TStringList` functions }

{ `RemoveComments`

  Strip out everything between a comment char and the next newline.
}
function RemoveComments(InputLines: TStringList): TStringList;
var
  I: Integer;
begin
  assert(InputLines <> nil);
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].StartsWith('%') then
      InputLines.Delete(I)
    else
      InputLines[I] := StringDropAfter(InputLines[I], '%');
  end;
  result := InputLines;
end;

{ `RemoveBlankLines`

  Delete lines that are empty or contain only whitespace from a `TStringList`.
}
function RemoveBlankLines(InputLines: TStringList): TStringList;
var 
  I: Integer;
begin
  assert(InputLines <> nil);
  for I := InputLines.Count - 1 downTo 0 do
  begin
    if InputLines[I].Trim.IsEmpty then
      InputLines.Delete(I);
  end;
  result := InputLines;
end;

{ # MAIN }
var
  InputText, OutputText: TStringList;
// LyHeader: TStringList;
//  HeaderValues: THeader;
//  Macros: TMacroDict;
//  Outline: TMacroOutline;
begin
  InputText     := TStringList.Create;
  OutputText    := TStringList.Create;
//  LyHeader      := TStringList.Create;
//  HeaderValues  := THeader.Create;
//  Macros        := TMacroDict.Create;
//  Outline       := TMacroOutline.Create;

  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: header INFILE.ly');
      exit;
    end
    else
      InputText.LoadFromFile(ParamStr(1));
    
    InputText := RemoveComments(InputText);

    OutputText := RemoveBlankLines(InputText);
    WriteLn(OutputText.Text);

  finally
    (*
//    FreeAndNil(Outline);
//    FreeAndNil(Macros);
//    FreeAndNil(HeaderValues);
//    FreeAndNil(LyHeader);
//    *)
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

