{$mode objfpc}
program charCodes;
uses SysUtils, Classes, Generics.Collections;

type
  TIntList = class(specialize TList<Integer>)
  public
    constructor Create(InputChars: String);
    function ToString: String; reintroduce;
  end;

constructor TIntList.Create(InputChars: String);
var
  ThisChar: Char;
  NewInt: Integer;
begin
  inherited Create;
  for ThisChar in InputChars do
  begin
    NewInt := Ord(ThisChar);
    Add(NewInt);
  end;
end;

function TIntList.ToString: String;
var
  ThisInt, Index: Integer;
  OutputStr: String;
begin
  OutputStr := '[';
  Index := 0;
  for ThisInt in Self do
  begin
    OutputStr := OutputStr + ThisInt.ToString;
    if Index < Count - 1 then
    begin
      OutputStr := OutputStr + ', ';
    end;
    Inc(Index);
  end;
  result := OutputStr + ']';
end;

{ MAIN }
var
  InputText: TStringList;
  InputStr: String;
  Codes: TIntList;
begin
  InputText := TStringList.Create;
  if (ParamCount = 1) then
  begin
    InputText.LoadFromFile(ParamStr(1));
    InputStr := InputText[0];
  end
  else
  begin
    ReadLn(InputStr);
  end;
  
  Codes := TIntList.Create(InputStr);
  WriteLn(Codes.ToString);
 
  FreeAndNil(Codes);
  FreeAndNil(InputText);
end.
