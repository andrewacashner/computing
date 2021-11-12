{$mode objfpc}{$H+}{$J-} 

program NewObjectInFunction;
{ can we make new objects in functions without too much trouble? }

uses SysUtils, Classes, Generics.Collections;

type
  TListOfTriples = specialize TObjectList<TStringList>;
function ListOfThree(A, B, C: String): TStringList;
var
  NewList: TStringList;
begin
  NewList := TStringList.Create;
  NewList.Add(A);
  NewList.Add(B);
  NewList.Add(C);
  result := NewList;
end;

var
  List: TListOfTriples;
  ThisTriple: TStringList;
begin
  List := TListOfTriples.Create;
  List.Add(ListOfThree('one', 'two', 'three'));
  List.Add(ListOfThree('uno', 'dos', 'tres'));
  for ThisTriple in List do
    WriteLn(ThisTriple.Text);
  FreeAndNil(List);
end.
