{$mode objfpc}{$H+}{$J-}

program kth(input, output, stderr);

uses SysUtils, LinkedList;

var 
  Nums: Array of Integer = (1, 2, 3, 4, 5, 6, 7);
  TestVals: Array of Integer = (0, 7, 2, 4, -2, -4);
  List: TLinkedList;
  Answer, TestVal: Integer;
begin
  try
    List := TLinkedList.CreateList(Nums);
    WriteLn(List.ToString());
    { 
    for TestVal in TestVals do
    begin
      try 
        WriteLn('At ' + IntToStr(TestVal) + ': ');
        Answer := List.At(TestVal);
        WriteLn(IntToStr(Answer));
      except 
        on E: Exception do WriteLn(E.Message);
      end;
    end;
    }
  finally
    FreeAndNil(List);
    {    FreeAndNil(List); }
  end;
end.
