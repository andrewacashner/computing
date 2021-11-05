{ # `ly2mei`

  Andrew Cashner

  A converter from Lilypond to MEI-XML, specifying a strict subset of
  Lilypond.

  # Changelog

  - 2021/10/07: Began
  - 2021/10/12: Macro-substitution program complete
  - 2021/10/13: Header conversion program complete
  - 2021/10/18: Macro-substitution functions revised and integrated
}
{$mode objfpc}{$H+}{$J-}
{$ASSERTIONS+}
{$OPTIMIZATION tailrec}

program ly2mei(input, output, stderr);

uses SysUtils, Classes, StrUtils, Generics.Collections;

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
  
  Cut out everything up to and including a given substring from a larger string.
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

{ `CopyStringRange`

  Return the portion of the string between the indices in a `TIndexPair`.
  The start and end characters of the range are included by default
  (`rkInclusive`). Return the original string if the index pair is invalid.
}
function CopyStringRange(Source: String; Outline: TIndexPair; 
                              ModeFlag: TRangeMode): String;
begin
  assert(Outline <> nil);
  if not Outline.IsValid then
    result := Source
  else
  begin
    case ModeFlag of
      rkInclusive: result := Source.Substring(Outline.FStart, Outline.Span);
      rkExclusive: result := Source.Substring(Outline.FStart + 1, 
                                Outline.Span - 2);
    end;
  end;
end;

{ `CutStringRange`

  Inverse of `CopyStringRange`: Return a string with the portion between
  the indices in a `TIndexPair` removed. 
}
function CutStringRange(Source: String; Outline: TIndexPair): String;
begin
  assert(Outline <> nil);
  if not Outline.IsValid then
    result := Source
  else
  begin
    result := Source.Substring(0, Outline.FStart - 1) 
              + Source.Substring(Outline.FEnd); 
  end;
end;

{ `BalancedDelimiterSubstring`

  In a string, mark the start and end indices of a single expression between
  given delimiter characters, ignoring any nested groups with the same
  delimiters in between. The delimiters must not be identical, otherwise it is
  impossible to determine nesting.
}
function BalancedDelimiterSubstring(Source: String; StartDelim, EndDelim:
  Char; Outline: TIndexPair): TIndexPair; 
var
  BraceLevel, SIndex: Integer;
  ThisChar: Char;
begin
  assert(StartDelim <> EndDelim);
  Outline.Clear;
  BraceLevel := 0;
  SIndex := 0;
  for ThisChar in Source do
  begin
    if ThisChar = StartDelim then
    begin
      if BraceLevel = 0 then
      begin
        Outline.FStart := SIndex;
      end;
      Inc(BraceLevel);
    end
    else
    begin 
      if ThisChar = EndDelim then
      begin
        if BraceLevel > 0 then
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
    end;
    Inc(SIndex);
  end; { for }

  result := Outline;
end;

function FindMatchedBraces(Source: String; Outline: TIndexPair): TIndexPair;
begin
  assert(Outline <> nil);
  result := BalancedDelimiterSubstring(Source, '{', '}', Outline);
end;

function CopyBraceExpr(Source: String): String;
var
  Outline: TIndexPair;
  TempStr: String;
begin
  Outline := TIndexPair.Create;
  try
    Outline := FindMatchedBraces(Source, Outline);
    if Outline.IsValid then
      TempStr := CopyStringRange(Source, Outline, rkInclusive)
    else 
      TempStr := '';
  finally
    FreeAndNil(Outline);
    result := TempStr;
  end;
end;



{ ## Parsing macro definitions and commands }

{ CLASS: `TCommandArg` for a command and its argument }
type
  TStatusCommandArg = (skCommand, skCommandArg, skInvalid);
  
  TCommandArg = class
  private
    FCommand, FArg: String;
    FStatus: TStatusCommandArg;
  public
    procedure Clear;
    function IsValid: Boolean;
    function ToString: String; override;
    function ExtractFromString(Source: String; ControlChar, ArgStartDelim,
      ArgEndDelim: Char): TCommandArg;
  end;

procedure TCommandArg.Clear;
begin
  FCommand := '';
  FArg := '';
  FStatus := skInvalid;
end;

function TCommandArg.IsValid: Boolean;
begin
  result := not (FStatus = skInvalid);
end;

function TCommandArg.ToString: String;
begin
  if Self.IsValid then
    result := FCommand + ' ' + FArg
  else
    result := '';
end;

{ `TCommandArg.ExtractFromString`

  In a string, find the first instance of command that starts with a given
  control character (e.g., backslash). If it is followed by an argument
  delimited by given strings (e.g., curly braces), return an object with both
  the command and the argument. If not return the object marked invalid.
  For instance, find the command `\markup < arg >` and 
  return `('\markup', '< arg >').
  The delimiters are included in the string.
}
function TCommandArg.ExtractFromString(Source: String; ControlChar, ArgStartDelim,
  ArgEndDelim: Char): TCommandArg;
var
  TestStr, Command: String;
  Outline: TIndexPair;
begin
  Outline := TIndexPair.Create;
  try
    Self.Clear;
    { find command }
    TestStr := Source.Substring(Source.IndexOf(ControlChar));
    Command := ExtractWord(1, TestStr, [' ', LineEnding, ArgStartDelim]);
    if not Command.IsEmpty then
    begin
      FCommand := Command;
      FStatus := skCommand;
      DebugLn('Found command ' + FCommand);

      { find arg within delimiters }
      TestStr := TestStr.Substring(Length(FCommand));
      DebugLn('After command ' + FCommand 
        + ', Ready to test string starting: ' + TestStr.Substring(0, 30));
      if TestStr.TrimLeft.StartsWith(ArgStartDelim) then
      begin
        DebugLn('Looking for command arg...');
        Outline := BalancedDelimiterSubstring(TestStr, ArgStartDelim,
                    ArgEndDelim, Outline); 
        if Outline.IsValid then
        begin
          FArg := CopyStringRange(TestStr, Outline, rkInclusive);
          DebugLn('Found argument: ' + FArg);
          FStatus := skCommandArg;
        end;
      end;
    end;
  finally
    FreeAndNil(Outline);
    result := Self;
  end;
end;

{ `LyArg`

  Find the first occurence of a given Lilypond command in a string and return
  its brace-delimited argument. Return an empty string if not found.
}
function LyArg(Source, Command: String): String;
var
  CommandArg: TCommandArg;
  Arg: String;
begin
  CommandArg := TCommandArg.Create;
  try
    if Source.Contains(Command) then
    begin
      Source := Source.Substring(Source.IndexOf(Command));
      CommandArg := CommandArg.ExtractFromString(Source, '\', '{', '}');
      if CommandArg.IsValid and (CommandArg.FCommand = Command) then
        Arg := CommandArg.FArg;
    end;
  finally
    FreeAndNil(CommandArg);
    result := Arg;
  end;
end;

{ CLASS: Macro dictionary of key-value pairs }
type
  TMacroDict = class(specialize TDictionary<String, String>)
  public
    function ToString: String; override;
    function ExtractMacros(InputText, OutputText: TStringList): TMacroDict;
  end;

  TMacroKeyValue = TMacroDict.TDictionaryPair;

{ `TMacroDict.ToString`

  Return the contents of a macro dictionary as a string, for testing/debugging
  purposes.
}
function TMacroDict.ToString: String;
var 
  Macro: TMacroKeyValue;
  OutputStr, MacroStr: String;
  N: Integer;
begin
  OutputStr := '';
  N := 1;
  for Macro in Self do
  begin
    MacroStr := IntToStr(N) + '. ' + Macro.Key + ': ' + Macro.Value + LineEnding;
    OutputStr := OutputStr + MacroStr;
    Inc(N);
  end;
  result := OutputStr;
end;
    
{ `FindReplaceMacros`

  In a given string (not a list), replace all macro commands (`\command`) with
  the corresponding definition in a macro dictionary. Repeat as necessary
  until all known macros are expanded.

  A macro call must be followed by a space or newline. Otherwise we could not
  have commands like \SopranoI and \SopranoII.
}
function FindReplaceMacros(Source: String; Dict: TMacroDict): String;
var
  OutputStr: String;
  Macro: TMacroKeyValue;
  HasMacros: Boolean;
begin
  OutputStr := Source;
  HasMacros := True;
  while HasMacros do
  begin
    for Macro in Dict do
    begin
      OutputStr := StringReplace(OutputStr, 
        Macro.Key + ' ', Macro.Value + ' ', [rfReplaceAll]);
      OutputStr := StringReplace(OutputStr, 
        Macro.Key + LineEnding, Macro.Value + LineEnding, [rfReplaceAll]);
    end;
    HasMacros := False;
    for Macro in Dict do
    begin
      if OutputStr.Contains(Macro.Key) then
      begin
        DebugLn('Source still contains macro key ' + Macro.Key );
        HasMacros := True;
      end;
    end;
  end;
  result := OutputStr;
end;


{ `TMacroDict.ExtractMacros`

  Find, parse, and save macro definitions in a stringlist. Return a macro
  dictionary; if no valid macros are found, it will be empty. Values may
  contain unexpanded macros.

  **Side effect:** The function also stores a copy of the input text minus
  macro definitions in the second argument, another stringlist.

  A macro must have the form `label = < arg >` or `label = \command < arg >`
  where `<>` are curly brackets. We don't accept `label = "string"` or other
  formats. The label must be at the beginning of a line.
}
function TMacroDict.ExtractMacros(InputText, OutputText: TStringList): TMacroDict;
var
  InputStr, BufferStr, ThisString, NextStr, Key, Value, TestStr: String;
  LineIndex: Integer;
  FindOutline, CopyOutline: TIndexPair;
  CommandArg: TCommandArg;
  Found: Boolean;
begin
  assert(InputText <> nil);
  assert(OutputText <> nil);

  FindOutline := TIndexPair.Create;
  CopyOutline := TIndexPair.Create;
  CommandArg := TCommandArg.Create;
  try
    LineIndex := 0;
    CopyOutline.FStart := 0;
    CopyOutline.FValid := True;
    BufferStr := '';
    InputStr := InputText.Text;
    { Look for a `key = value` pair at the start of each line }
    for ThisString in InputText do
    begin
      Found := False;
      if ThisString.Contains('=') and not ThisString.StartsWith(' ') then
      begin
        InputText.GetNameValue(LineIndex, Key, Value);
        if Key.IsEmpty or Value.IsEmpty then
          continue;
        
        { Found key, mark start location }
        DebugLn('Found possible macro: ' + Key + ' | ' + Value);
        Key := Key.Trim;
        CopyOutline.FEnd := InputStr.IndexOf(Key + ' ');

        { Parse value }
        Value := Value.Trim;
        case Value.Substring(0, 1) of
        '{':
          { Value is a brace-delimited argument }
          begin
            TestStr := ListToStringFromIndex(InputText, LineIndex);
            FindOutline := FindMatchedBraces(TestStr, FindOutline);
            if FindOutline.IsValid then
            begin
              Value := CopyStringRange(TestStr, FindOutline, rkInclusive);
              Found := True;
            end;
          end;

        '\':
          { Value is a command, possibly followed by argument }
          begin
            TestStr := ListToStringFromIndex(InputText, LineIndex);
            CommandArg := CommandArg.ExtractFromString(TestStr, '\', '{', '}');
            case CommandArg.FStatus of
            { Found only a command }
            skCommand:
              begin
                Value := CommandArg.FCommand;
                Found := True;
              end;
            { Found a command and a brace-delimited argument }
            skCommandArg:
              begin
                Value := CommandArg.ToString;
                Found := True;
              end;
            end;
          end;
        end;
        { Add found key/value pair to dictionary; copy text from last ending
        position to next start position to output; mark new start position }
        if Found then
        begin
          Self.Add('\' + Key, Value);
          NextStr := InputStr.Substring(CopyOutline.FStart, CopyOutline.Span);
          if not NextStr.IsNullOrWhitespace(NextStr) then
          begin
            BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart,
              CopyOutline.Span);
              DebugLn('Copying from index ' + IntToStr(CopyOutline.FStart) 
              + ' to ' + IntToStr(CopyOutline.FEnd)); 
          end;
          CopyOutline.FStart := InputStr.IndexOf(Value) + Length(Value);
        end;
      end;
      Inc(LineIndex);
    end;
    { Add remaining text after last macro definition to output }
    BufferStr := BufferStr + InputStr.Substring(CopyOutline.FStart);
    OutputText := Lines(BufferStr, OutputText);

  finally
    FreeAndNil(CommandArg);
    FreeAndNil(CopyOutline);
    FreeAndNil(FindOutline);
    result := Self;
  end;
end;

{ `ExpandMacros`

  Process all macros in source text
}
function ExpandMacros(InputText: TStringList): TStringList;
var
  Macros: TMacroDict;
  TempLines: TStringList;
  TempStr: String;
begin
  Macros := TMacroDict.Create;
  TempLines := TStringList.Create;
  try
    Macros := Macros.ExtractMacros(InputText, TempLines);
    DebugLn('Macro dictionary: ' + LineEnding + Macros.ToString);

    TempStr   := FindReplaceMacros(TempLines.Text, Macros);
    TempLines := RemoveBlankLines(Lines(TempStr, TempLines));
    InputText.Assign(TempLines);
  finally
    FreeAndNil(Macros);
    FreeAndNil(TempLines);
    result := InputText;
  end;
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
        Markup := CopyStringRange(Source, Outline, rkExclusive);
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
  ThisString, Key, Value, MarkupStr: String;
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
          MarkupStr := LyArg(Value, '\markup');
          Value := ExtractQuotedStrings(MarkupStr);
          FoundThis := True;
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

  MEI.Add('<meiHead>');
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
  
  if not FSource.IsEmpty then
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
  SearchStr: String;
  Outline: TIndexPair;
begin
  LyHeader := TStringList.Create;
  Outline := TIndexPair.Create;
  HeaderValues.FValid := False;
  try
    SearchStr := LyArg(InputText.Text, '\header');
    if not SearchStr.IsEmpty then
    begin
      LyHeader := Lines(SearchStr, LyHeader);
      HeaderValues := HeaderValues.FromLily(LyHeader);
    end;
  finally
    FreeAndNil(Outline);
    FreeAndNil(LyHeader);
    result := HeaderValues;
  end;
end;


{ Types for Lilypond `\new XX < YY >` expressions }
type
  TLyObject = class
  private
    var
      FType, FID, FContents: String;
      FChild, FSibling: TLyObject;
  public
    constructor Create();
    constructor Create(TypeStr, IDStr, ContentsStr: String);
    constructor Create(TypeStr, IDStr, ContentsStr: String; 
      Child, Sibling: TLyObject);
    destructor Destroy; override;
    function AddChild(NewObject: TLyObject): TLyObject;
    function AddSibling(NewObject: TLyObject): TLyObject;
    function LastChild: TLyObject;
    function LastSibling: TLyObject;
    function ToString: String; override;
  end;

constructor TLyObject.Create();
begin
  inherited Create;
end;

constructor TLyObject.Create(TypeStr, IDStr, ContentsStr: String);
begin
  inherited Create;
  FType     := TypeStr;
  FID       := IDStr;
  FContents := ContentsStr;
  FChild    := nil;
  FSibling  := nil;

end;

constructor TLyObject.Create(TypeStr, IDStr, ContentsStr: String;
  Child, Sibling: TLyObject);
begin
  inherited Create;
  FType     := TypeStr;
  FID       := IDStr;
  FContents := ContentsStr;
  FChild    := Child;
  FSibling  := Sibling;
end;

destructor TLyObject.Destroy;
begin
  if FChild <> nil then 
    FChild.Destroy;
  if FSibling <> nil then 
    FSibling.Destroy;
  inherited Destroy;
end;

function TLyObject.AddChild(NewObject: TLyObject): TLyObject;
begin
  FChild := NewObject;
  result := Self;
end;

function TLyObject.AddSibling(NewObject: TLyObject): TLyObject;
begin
  FSibling := NewObject;
  result := Self;
end;

function TLyObject.LastChild: TLyObject;
begin
  if FChild = nil then
    result := Self
  else
    result := FChild.LastChild;
end;


function TLyObject.LastSibling: TLyObject;
begin
  if FSibling = nil then
    result := Self
  else
    result := FSibling.LastSibling;
end;

function TLyObject.ToString: String; 
  function TreeToString(Parent: TLyObject; Generation: Integer): String;
  var
    Indent: String;
    ParentStr, ChildStr, SibStr, IDStr: String;
  begin
    if Parent <> nil then
    begin
      Indent := StringOfChar(' ', 2 * Generation);

      if Parent.FID <> '' then 
        IDStr := '" id="' + Parent.FID;
      
      ParentStr := 'type="' + Parent.FType + IDStr + '">' + Parent.FContents;

      if Parent.FChild <> nil then
        ChildStr := LineEnding + TreeToString(Parent.FChild, Generation + 1) + Indent;

      SibStr := LineEnding;
      if Parent.FSibling <> nil then 
        SibStr := SibStr + TreeToString(Parent.FSibling, Generation);

      result := Indent + '<lyobject ' + ParentStr + ChildStr + '</lyobject>' + SibStr;
    end;
  end;
begin
  if FType = '' then
    result := ''
  else 
    result := TreeToString(Self, 0);
end;

{ `FindLyNewTree`

  Find the argument of a Lilypond `\new` command 
  (e.g., `\new Voice = "SI" < \MusicSI >`
    => `LyObject < type = "Voice", id = "SI", contents = "< \MusicSI >" >`
}
function FindLyNewTree(Source: String; Tree: TLyObject): TLyObject;
var
  SearchIndex: Integer;
  SearchStr: String;
  ThisType, ThisID, ThisContents: String;
  Outline: TIndexPair;
  EntryNestLevel, CurrentNestLevel: Integer;
  InsideAngleBrackets: Boolean;
begin
  Outline := TIndexPair.Create;
  try
    SearchIndex := 0;
    SearchStr := Source;
    EntryNestLevel := 0;
    InsideAngleBrackets := False;
    while (Length(SearchStr) > 0) and (SearchIndex <> -1) do
    begin
      CurrentNestLevel := EntryNestLevel;
      SearchIndex := SearchStr.IndexOf('\new ');
      if SearchIndex = -1 then break;
      
      DebugLn('Found a \new expression at line ' + IntToStr(SearchIndex));
     
      { Find Type: `\new Voice ...` => `Voice` }
      SearchStr := SearchStr.Substring(SearchIndex);
      ThisType := ExtractWord(2, SearchStr, StdWordDelims);
      SearchStr := StringDropBefore(SearchStr, ThisType);
   
      { Find ID: `\new Voice = "Soprano"` => 'Soprano' }
      if SearchStr.StartsWith(' = "') then
      begin 
        SearchStr := StringDropBefore(SearchStr, ' = "');
        ThisID := SearchStr.Substring(0, SearchStr.IndexOf('"'));
        SearchStr := StringDropBefore(SearchStr, ThisID + '"');
      end
      else
        ThisID := '';
   
      { Find Contents: Either an expression within `<<...>>` (here meaning
      actually angle brackets), or an expression within curly braces and everything preceding it:
        e.g., `\new Lyrics \lyricsto "Alto" < \LyricsA >`
          => `\lyricsto "Alto" < \LyricsA >` (here meaning curly braces)
      }
      if SearchStr.TrimLeft.StartsWith('<<') then
      begin
        InsideAngleBrackets := True;
        Inc(CurrentNestLevel);
        Outline := BalancedDelimiterSubstring(SearchStr, '<', '>', Outline);
        ThisContents := CopyStringRange(SearchStr, Outline, rkInclusive);
        { look for \new objects within the <<...>> range }
        { add them as children to the tree }
      end
      else
      begin
        InsideAngleBrackets := False;
        ThisContents := SearchStr.Substring(0, SearchStr.IndexOf('{'));
        ThisContents := ThisContents + CopyBraceExpr(SearchStr);
      end;

      if Tree = nil then
        Tree := TLyObject.Create(ThisType, ThisID, '')
      else
      begin
        if CurrentNestLevel > EntryNestLevel then
        begin
          Tree.LastChild.FChild := TLyObject.Create(ThisType, ThisID, '');
          Dec(CurrentNestLevel);
        end
        else
          Tree.LastSibling.FSibling := TLyObject.Create(ThisType, ThisID, '');
      end;
      if InsideAngleBrackets then
        Tree.LastChild.FChild := FindLyNewTree(ThisContents, nil);

      { Skip past '\new ' to look for next}
      Source := Source.Substring(SearchIndex + 5); 
      end; 
  finally
    FreeAndNil(Outline);
    result := Tree;
  end;
end;

{ # MAIN }
const
  XMLversion = '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding;
  MeiNamespace = 'xmlns="http://www.music-encoding.org/ns/mei"';
var
  InputText, OutputText: TStringList;
  ScoreInput, MEIheader, MEIscore, MEI: String;
  HeaderValues: THeader;
  CommandArg: TCommandArg;
  LyObjectTree: TLyObject;
begin
  InputText     := TStringList.Create;
  OutputText    := TStringList.Create;
  HeaderValues  := THeader.Create;
  CommandArg    := TCommandArg.Create;
  LyObjectTree  := nil;

  try
    if ParamCount <> 1 then
    begin
      WriteLn(stderr, 'Usage: header INFILE.ly');
      exit;
    end
    else
      InputText.LoadFromFile(ParamStr(1));

    InputText := RemoveComments(InputText);
    InputText := RemoveBlankLines(InputText);

    { process macros: find and cut defs, expand macro commands }
    InputText := ExpandMacros(InputText);
    DebugLn('Input after macro expansion: ' + InputText.Text);

    { process header, convert to MEI }
    HeaderValues := ParseHeader(InputText, HeaderValues);
    if not HeaderValues.IsValid then
    begin
      WriteLn(stderr, 'Did not find a valid header definition');
      exit;
    end
    else
      MEIheader := HeaderValues.ToMei(OutputText).Text;
    
    { process score, convert to MEI }
    ScoreInput := LyArg(InputText.Text, '\score');
    if ScoreInput.IsEmpty then
    begin
      WriteLn(stderr, 'Did not find a valid \score expression');
      exit;
    end
    else
      DebugLn('Found score expression: ' + ScoreInput);

    LyObjectTree := FindLyNewTree(ScoreInput, LyObjectTree);

    if LyObjectTree = nil then
      MEIscore := '<score></score>' + LineEnding
    else
      MEIscore := LyObjectTree.ToString;

    { output }
    MEI := XMLversion + '<mei ' + MEINamespace + '>' + LineEnding 
            + MEIheader + MEIscore
            + '</mei>';
    WriteLn(MEI);

  finally
    FreeAndNil(LyObjectTree);
    FreeAndNil(CommandArg);
    FreeAndNil(HeaderValues);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

