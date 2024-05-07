{$mode objfpc}{$H+}{$J-}

program perm(input, output);

uses SysUtils, Generics.Collections;

type
  CharDict = specialize TDictionary<Char, Integer>;

procedure RegisterChars(Dict: CharDict; InputStr: String; Increment: Integer);
var
  ThisChar: Char;
  Count: Integer;
begin
  for ThisChar in InputStr do
  begin
    if Dict.TryGetValue(ThisChar, Count) then
      Dict.AddOrSetValue(ThisChar, Count + Increment)
    else
      Dict.AddOrSetValue(ThisChar, Increment);
  end;
end;

function IsPermutation(StringA, StringB: String): Boolean;
var
  Inventory: CharDict;
  ThisValue: Integer;
  Count: Integer = 0;
begin
  Inventory := CharDict.Create();
  try
    RegisterChars(Inventory, StringA, 1);
    RegisterChars(Inventory, StringB, -1);
    for ThisValue in Inventory.Values do
      Inc(Count, ThisValue);

  finally
    FreeAndNil(Inventory);
  end;
  result := Count = 0;
end;

function TestMsg(StringA, StringB: String): String;
begin
  result := StringA + ' vs. ' + StringB + ': ' + BoolToStr(IsPermutation(StringA, StringB), True);
end;

var
  TestValues: Array of Array of String =
    (('abba', 'baab'),
     ('dad', 'add'),
     ('bacon', 'eggs'),
     ('shut', 'tush'));

  ThisPair: Array of String;
  Msg: String = '';

begin
  for ThisPair in TestValues do
      Msg := Msg + TestMsg(ThisPair[0], ThisPair[1]) + sLineBreak;

  WriteLn(Msg);
end.



