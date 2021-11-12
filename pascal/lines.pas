{$mode objfpc}{$H+}{$J-}{$ASSERTIONS+}
program Lines;

uses SysUtils, Classes;

type
  TMyStringList = class(TStringList)
  public
    constructor Create(InputStr: String);
  end;

constructor TMyStringList.Create(InputStr: String);
begin
  inherited Create;
  Delimiter := LineEnding;
  StrictDelimiter := True;
  DelimitedText := InputStr;
end;

var
  InputStr: String = 'hello' + LineEnding + 'world' + LineEnding + 'how are' 
    + LineEnding + 'you?' + LineEnding;
  OutputLines: TMyStringList;
begin
  OutputLines := TMyStringList.Create(InputStr);
  Write(OutputLines.Text);
  FreeAndNil(OutputLines);
end.


