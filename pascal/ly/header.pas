{ `header`

  Andrew Cashner, 2021/10/13

  Translate a Lilypond header expression to MEI-XML.
}
{$mode objfpc}{$H+}{$J-}
program header(input, output, stderr);

uses SysUtils, Classes;

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
    function Clear: TIndexPair;
    function Span: Integer;
  end;

{ `TIndexPair` class methods }
function TIndexPair.Clear: TIndexPair;
begin
  FStart := 0;
  FEnd   := 0;
  FValid := False;
  result := Self;
end;

function TIndexPair.Span: Integer;
begin
  if FValid then
    result := FEnd - FStart
  else
    result := -1;
end;

{ `MatchBraces`

  In a string, mark the start and end indices of a single expression delimited
  by balanced curly braces, which may including other such expressions. 
}
function MatchBraces(Source: String; Outline: TIndexPair): TIndexPair;
type
  TReadMode = (rkNormal, rkBrace);
var
  BraceLevel, SIndex: Integer;
  Current: String;
  ReadMode: TReadMode;
begin
  Outline := Outline.Clear;
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

{ `StringDropBefore`
  
  Cut out everything up to a given substring from a larger string.
}
function StringDropBefore(Source, Cut: String): String;
begin
  result := Source.Substring(Source.IndexOf(Cut) + Length(Cut));
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
  { XXX could subtitle be optional? }
  public
    function ToMEI(MEI: TStringList): TStringList;
  end;

{ TODO make this a class function of THeader (e.g., THeader.PopulateFromString) }
{ `ParseHeader`

  Read a Lilypond header and extract specific values.
}
function ParseHeader(LyHeaderStr: String; HeaderValues: THeader): THeader; 
begin
  result := HeaderValues; { XXX temp }
end;


{ `THeader class functions }
function THeader.ToMEI(MEI: TStringList): TStringList;
begin
  if MEI = nil then
  begin
    WriteLn(stderr, 'Function THeader requires an initialized TStringList');
    exit;
  end;

  MEI.Add('<meiHead xmlns="http://www.music-encoding.org/ns/mei">');
  MEI.Add('  <fileDesc>');
  MEI.Add('    <titleStmt>');
  MEI.Add('      <title type="main">' + FTitle + '</title>');
  MEI.Add('      <title type="subtitle">' + FSubtitle + '</subtitle>');
  MEI.Add('      <respStmt>');
  MEI.Add('        <composer>' + FComposer + ' ' + FDates + '</composer>');
  MEI.Add('        <lyricist>' + FPoet + '</lyricist>');
  MEI.Add('        <editor>' + FEditor + '</editor>');
  MEI.Add('      </respStmt>');
  MEI.Add('    </titleStmt>');
  MEI.Add('    <editionStmt>');
  MEI.Add('      <respStmt><p>Edited by ' + FEditor + '</p></respStmt>');
  MEI.Add('    </editionStmt>');
  MEI.Add('    <pubStmt>');
  MEI.Add('      <availability><p>' + FCopyright + '</p></availability>');
  MEI.Add('    </pubStmt>');
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


{ MAIN }
var
  InputText, OutputText: TStringList;
  SearchStr, LyHeaderStr: String;
  Outline: TIndexPair;
  HeaderValues: THeader;
begin
  { setup }
  InputText := TStringList.Create;
  OutputText := TStringList.Create;
  Outline := TIndexPair.Create;
  HeaderValues := THeader.Create;

  try
    { read input }
    if ParamCount <> 1 then
    begin
      WriteLn('Usage: header INFILE.ly');
      exit;
    end;
    InputText.LoadFromFile(ParamStr(1));
    
    { find header }
    if InputText.Text.Contains('\header') then
    begin
      SearchStr := StringDropBefore(InputText.Text, '\header');
      Outline := MatchBraces(SearchStr, Outline);
      LyHeaderStr := SearchStr.Substring(Outline.FStart, Outline.Span);
    end;

    { find subfields and store }
    HeaderValues := ParseHeader(LyHeaderStr, HeaderValues);
    
    { make output }
    OutputText := HeaderValues.ToMei(OutputText); 
    WriteLn(OutputText.Text);

  finally
    FreeAndNil(HeaderValues);
    FreeAndNil(Outline);
    FreeAndNil(OutputText);
    FreeAndNil(InputText);
  end;
end.

