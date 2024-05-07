{$mode objfpc}{$H+}{$J-}

program mwi;

uses SysUtils, LinkedListMWI;
var
  List: TLinkedList;
begin
  List := TLinkedList.Create([1, 2, 3, 4, 5, 6, 7]);
  FreeAndNil(List);
end.
