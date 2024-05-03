{$mode objfpc}{$H+}{$J-}

program mwi;

uses SysUtils, LinkedListMWI;

var 
  Nums: Array of Integer = (1, 2, 3, 4, 5, 6, 7);
  List: TLinkedList;
begin
  List := TLinkedList.Create(Nums);
  FreeAndNil(List);
end.
