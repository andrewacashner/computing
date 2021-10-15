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

uses SysUtils, Classes, StrUtils;

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

{ # String methods }

{ `StringDropBefore`
  
  Cut out everything up to a given substring from a larger string.
}
function StringDropBefore(Source, Cut: String): String;
begin
  result := Source.Substring(Source.IndexOf(Cut) + Length(Cut));
end;

{ `StringDropAfter`
  
  Return the portion of a string before a given delimiter.
}
function StringDropAfter(InputStr: String; Delim: String): String;
begin
  if InputStr.Contains(Delim) then
    InputStr := InputStr.Substring(0, InputStr.IndexOf(Delim));
  result := InputStr;
end;

function IsASingleQuotedString(Source: String): Boolean;
begin
  result := (Source.CountChar('"') = 2)
            and Source.StartsWith('"') 
            and Source.EndsWith('"');
end;

{ # `TStringList` methods }

{ `Lines`

  Split a string at newlines to make a `TStringList` }
function Lines(InputStr: String; OutputList: TStringList): TStringList;
begin
  OutputList.Clear;
  OutputList.Delimiter := LineEnding;
  OutputList.StrictDelimiter := True;
  OutputList.DelimitedText := InputStr;
  result := OutputList;
end;

{ `ListToStringFromIndex`
  
  Return a string consisting of the text of a stringlist starting at a given
  index.
}
function ListToStringFromIndex(List: TStringList; Index: Integer): String;
begin
    result := List.Text.Substring(List.Text.IndexOf(List[Index]));
end;


{ `RemoveComments`

  Strip out everything between a comment char and the next newline.
}
function RemoveComments(InputLines: TStringList): TStringList;
var
  ThisString: String;
  TempLines: TStringList;
begin
  assert(InputLines <> nil);
  TempLines := TStringList.Create;
  try
    for ThisString in InputLines do
    begin
      if not ThisString.StartsWith('%') then
        TempLines.Add(StringDropAfter(ThisString, '%'));
    end;
    InputLines.Assign(TempLines);
  finally
    FreeAndNil(TempLines);
    result := InputLines;
  end;
end;

{ `RemoveBlankLines`

  Delete lines that are empty or contain only whitespace from a `TStringList`.
}
function RemoveBlankLines(InputLines: TStringList): TStringList;
var 
  ThisString: String;
  TempLines: TStringList;
begin
  assert(InputLines <> nil);
  TempLines := TStringList.Create;
  try
    for ThisString in InputLines do
    begin
      if not ThisString.Trim.IsEmpty then
        TempLines.Add(ThisString);
    end;
    InputLines.Assign(TempLines);
  finally
    FreeAndNil(TempLines);
    result := InputLines;
  end;
end;

{ # Parsing }

{ CLASS: `TIndexPair`

  Stores start and end positions in a sequence (as in a string).
}
type 
  TIndexPair = class
  private
    var
      FStart, FEnd: Integer;
      FValid: Boolean;
  public
    procedure Clear;
    function IsValid: Boolean;
    function Span: Integer;
    function FindRangeInString(Source, StartDelim, 
                                EndDelim: String): TIndexPair; 
  end;

{ ## `TIndexPair` class methods }

procedure TIndexPair.Clear;
begin
  FStart := 0;
  FEnd   := 0;
  FValid := False;
end;

function TIndexPair.IsValid: Boolean;
begin
  result := FValid;
end;

function TIndexPair.Span: Integer;
begin
  if FValid then
    result := FEnd - FStart
  else
    result := -1;
end;

{ `TIndexPair.FindRangeInString`

  Given a string, return a new instance containing the start and end indices
  of the range between the given delimiters. If not found, mark as invalid.
}
function TIndexPair.FindRangeInString(Source, StartDelim, 
                                          EndDelim: String): TIndexPair;
begin
  FStart := Source.IndexOf(StartDelim);
  FEnd   := Source.Substring(FStart + 1).IndexOf(EndDelim);
  FValid := not ((FStart = -1) or (FEnd = -1));
  FEnd   := FStart + FEnd;
  result := Self;
end;

type
  TRangeMode = (rkInclusive, rkExclusive);

{ `ExtractStringRange`

  Return the portion of the string between the indices in a `TIndexPair`.
  The start and end characters of the range are included by default
  (`rkInclusive`). Return the original string if the index pair is invalid.
}
function ExtractStringRange(Source: String; Outline: TIndexPair; 
                              ModeFlag: TRangeMode): String;
begin
  assert(Outline <> nil);
  if not Outline.IsValid then
    result := Source
  else
  begin
    case ModeFlag of
      rkInclusive: result := Source.Substring(Outline.FStart, Outline.Span);
      rkExclusive: result := Source.Substring(Outline.FStart + 1, Outline.Span - 2);
    end;
  end;
end;

{ `MatchBraces`

  In a string, mark the start and end indices of a single expression delimited
  by balanced curly braces, which may including other such expressions. 
}
function FindMatchedBraces(Source: String; Outline: TIndexPair): TIndexPair;
type
  TReadMode = (rkNormal, rkBrace);
var
  BraceLevel, SIndex: Integer;
  Current: String;
  ReadMode: TReadMode;
begin
  Outline.Clear;
  BraceLevel := 0;
  SIndex := 0;
  ReadMode := rkNormal;

  for Current in Source do
  begin
    case Current of
      '{':
      begin
        if ReadMode = rkNormal then
        begin
          Outline.FStart := SIndex;
          ReadMode := rkBrace;
        end;
        Inc(BraceLevel);
      end;

      '}':
      begin
        if ReadMode = rkBrace then
        begin
          Dec(BraceLevel);
          if BraceLevel = 0 then
          begin
            Outline.FEnd := SIndex + 1; { include closing brace }
            Outline.FValid := True;
            break;
          end;
        end;
      end;
    end; { case }
    Inc(SIndex);
  end; { for }
  result := Outline;
end;

{ ## Parsing the header }

{ `ExtractQuotedStrings`

  Find all the quoted portions of a given string and return them as a single
  string, delimited by spaces.
}
function ExtractQuotedStrings(Source: String): String;
var
  MarkupStrings: TStringList;
  Markup: String;
  Outline: TIndexPair;
begin
  MarkupStrings := TStringList.Create;
  Outline := TIndexPair.Create;
  try
    while Source.CountChar('"') > 1 do
    begin
      Outline := Outline.FindRangeInString(Source, '"', '"');
      if Outline.IsValid then
      begin
        Markup := ExtractStringRange(Source, Outline, rkExclusive);
        MarkupStrings.Add(Markup);
        Source := Source.Substring(Outline.FEnd + 2);
      end
      else
        break;
    end;
    MarkupStrings.StrictDelimiter := True;
    MarkupStrings.Delimiter := ' ';
    Source := DelChars(MarkupStrings.DelimitedText, '"');

  finally
    FreeAndNil(MarkupStrings);
    FreeAndNil(Outline);
    result := Source;
  end;
end;

type
  TCommandArg = class
  private
    FCommand, FArg: String;
    FValid: Boolean;
  public
    procedure Clear;
  end;

procedure TCommandArg.Clear;
begin
  FCommand := '';
  FArg := '';
  FValid := False;
end;

{ `ExtractCommandArg`

  In a stringlist, find the first instance of command that starts with a given
  control character (e.g., backslash). If it is followed by an argument
  delimited by given strings (e.g., curly braces), return an object with both
  the command and the argument. If not return the object marked invalid.
  For instance, find the command `\markup < arg >` and 
  return `('\markup', '< arg >').
  The delimiters are included in the string.
}
  { TODO just use a string input, you don't need lines. }
function ExtractCommandArg(Source: TStringList; ControlChar, ArgStartDelim,
  ArgEndDelim: Char; CommandArg: TCommandArg): TCommandArg;
var
  Command, Arg, TestStr: String;
  ListIndex, StrIndex: Integer;
begin
  CommandArg.Clear;
  ListIndex := Source.IndexOf(ControlChar);
  TestStr := Source[ListIndex];
  StrIndex := TestStr.IndexOf(ControlChar);
  Command := ExtractWord(1, TestStr.Substring(StrIndex), [' ', ArgStartDelim]);
  CommandArg.FCommand := ControlChar + Command;
  { find arg - match braces function but with given delimiters }

  result := CommandArg;
end;


{ CLASS: `THeader`

  Stores all the fields required in the Lilypond source file and can write
  them to MEI.
}
type
  THeader = class
  private
    const
      FProgramName: String = 'ly2mei';
    var
      FTitle, FSubtitle, FComposer, FDates, 
      FPoet, FEditor, FCopyright, FSource: String;
      FValid: Boolean;
  public
    function IsValid: Boolean;
    function FromLily(LyHeader: TStringList): THeader; 
    function ToMEI(MEI: TStringList): TStringList;
  end;

{ ## `THeader` class methods }

function THeader.IsValid: Boolean;
begin
  result := FValid;
end;


{ `THeader.FromLily`

  Read a Lilypond header and extract specific values.
}
function THeader.FromLily(LyHeader: TStringList): THeader; 
var 
  ThisString, Key, Value: String;
  Outline: TIndexPair;
  LineIndex: Integer;
  FoundThis, FoundAny: Boolean;
begin
  Outline := TIndexPair.Create;
  try
    LineIndex := 0;
    FoundAny := False;
    for ThisString in LyHeader do
    begin
      FoundThis := False;
      if ThisString.Contains('=') then
      begin
        LyHeader.GetNameValue(LineIndex, Key, Value);
        Key := Key.Trim;
        Value := Value.Trim;
       
        if IsASingleQuotedString(Value) then 
        begin
          Value := Value.DeQuotedString('"');
          FoundThis := True;
        end
        else
        begin
          if Value.StartsWith('\markup') then
          begin
            Value := ListToStringFromIndex(LyHeader, LineIndex);
            Value := StringDropBefore(Value, '\markup');
            
            if not Value.TrimLeft.StartsWith('{') then
              Value := ''
            else
            begin
              Outline := FindMatchedBraces(Value, Outline);
              Value := ExtractStringRange(Value, Outline, rkExclusive);
              Value := ExtractQuotedStrings(Value);
              FoundThis := True;
            end; { braces }
          end; { \markup }
        end;
      end; { if contains '=' }

      if FoundThis then
      begin
        case Key of
          'title':     FTitle := Value;
          'subtitle':  FSubtitle := Value;
          'composer':  FComposer := Value;
          'dates':     FDates := Value;
          'poet':      FPoet := Value;
          'editor':    FEditor := Value;
          'copyright': FCopyright := Value;
          'source':    FSource := Value;
        end;
        FoundAny := True;
      end;
      Inc(LineIndex);
    end;
    Self.FValid := FoundAny;
  finally
    FreeAndNil(Outline);
    result := Self;
  end;
end;


{ `THeader.ToMEI`

  Return a string list containing the MEI expression of the header data.
}
function THeader.ToMEI(MEI: TStringList): TStringList;
begin
  assert(MEI <> nil);
  MEI.Clear;

  MEI.Add('<meiHead xmlns="http://www.music-encoding.org/ns/mei">');
  MEI.Add('  <fileDesc>');
  MEI.Add('    <titleStmt>');
  MEI.Add('      <title type="main">' + FTitle + '</title>');

  if not FSubtitle.IsEmpty then
    MEI.Add('      <title type="subtitle">' + FSubtitle + '</subtitle>');

  MEI.Add('      <respStmt>');
  MEI.Add('        <composer>' + FComposer + ' ' + FDates + '</composer>');

  if not FPoet.IsEmpty then
    MEI.Add('        <lyricist>' + FPoet + '</lyricist>');

  if not FEditor.IsEmpty then
    MEI.Add('        <editor>' + FEditor + '</editor>');

  MEI.Add('      </respStmt>');
  MEI.Add('    </titleStmt>');
  
  if not FEditor.IsEmpty then
  begin
    MEI.Add('    <editionStmt>');
    MEI.Add('      <respStmt><p>Edited by ' + FEditor + '</p></respStmt>');
    MEI.Add('    </editionStmt>');
  end;

  if not FCopyright.IsEmpty then
  begin
    MEI.Add('    <pubStmt>');
    MEI.Add('      <availability><p>' + FCopyright + '</p></availability>');
    MEI.Add('    </pubStmt>');
  end;

  MEI.Add('  </fileDesc>');
  MEI.Add('  <encodingDesc>');
  MEI.Add('    <appInfo>');
  MEI.Add('      <application><name>' + FProgramName + '</name></application>');
  MEI.Add('    </appInfo>');
  MEI.Add('  </encodingDesc>');
  MEI.Add('  <sourceDesc><source>' + FSource + '</source></sourceDesc>');
  MEI.Add('</meiHead>'); 

  result := MEI;
end;


{ `ParseHeader`

  Find a header definition and parse it into a `THeader` object.
}
function ParseHeader(InputText: TStringList; HeaderValues: THeader): THeader;
var
  LyHeader: TStringList;
  SearchStr, LyHeaderStr: String;
  Outline: TIndexPair;
begin
  LyHeader := TStringList.Create;
  Outline := TIndexPair.Create;
  HeaderValues.FValid := False;
  try
    if InputText.Text.Contains('\header') then
    begin
      SearchStr := StringDropBefore(InputText.Text, '\header');
      Outline := FindMatchedBraces(SearchStr, Outline);
      if Outline.IsValid then
      begin
        LyHeaderStr := ExtractStringRange(SearchStr, Outline, rkExclusive);
        LyHeader := Lines(LyHeaderStr, LyHeader);
        HeaderValues := HeaderValues.FromLily(LyHeader);
      end;
    end;
  finally
    FreeAndNil(Outline);
    FreeAndNil(LyHeader);
    result := HeaderValues;
  end;
end;



{ # MAIN }
var
  InputText, OutputText: TStringList;
  HeaderValues: THeader;
  CommandArg: TCommandArg;
begin
  InputText     := TStringList.Create;
  OutputText    := TStringList.Create;
  HeaderValues  := THeader.Create;
  CommandArg    := TCommandArg.Create;
  
  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: header INFILE.ly');
      exit;
    end
    else
      InputText.LoadFromFile(ParamStr(1));

    InputText := RemoveComments(InputText);

    { process macros }
    InputText := RemoveBlankLines(InputText);

    { process header }
    HeaderValues := ParseHeader(InputText, HeaderValues);
    if not HeaderValues.IsValid then
    begin
      WriteLn(stderr, 'Did not find a valid header definition');
      exit;
    end
    else
      OutputText := HeaderValues.ToMei(OutputText);
   
    { process score }

    { output }
    WriteLn(OutputText.Text);

    CommandArg := ExtractCommandArg(InputText, '\', '{', '}', CommandArg);
    WriteLn('Found command : ' + CommandArg.FCommand);

  finally
    FreeAndNil(CommandArg);
    FreeAndNil(HeaderValues);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

