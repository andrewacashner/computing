{$mode objfpc}{$H+}{$J-}{$optimization tailrec}

program SimpleList;
uses SysUtils;

type
  TPNode = ^TNode;
  TNode = record
    FData: Integer;
    FPNext: TPNode;
  end;

function Last(List: TPNode): TPNode;
begin
  if List^.FPNext = nil then
    begin
      result := List;
    end
  else
    begin
      result := Last(List^.FPNext);
    end;
end;

{ Create a new node; if given list is empty, return a new list with that node;
  otherwise add it to the end; return the head of the list }
function AddNode(Head: TPNode; Num: Integer): TPNode;
var
  NewNode: TPNode;
  ListEnd: TPNode = nil;
begin
  new(NewNode);
  NewNode^.FData := Num;
  NewNode^.FPNext := nil;

  if Head = nil then
    begin
      Head := NewNode;
    end
  else
    begin
      ListEnd := Last(Head);
      ListEnd^.FPNext := NewNode;
    end;
  result := Head;
end;

procedure FreeList(Head: TPNode);
begin
  if Head <> nil then
    begin
      FreeList(Head^.FPNext);
      dispose(Head);
    end;
end;

function ListToString(Head: TPNode; Msg: String): String;
begin
  if Head = nil then
    begin
      result := Msg;
    end
  else
    begin
      Msg := Msg + IntToStr(Head^.FData) + ' ';
      result := ListToString(Head^.FPNext, Msg);
    end;
end;

{ MAIN }
var
  PList: TPNode = nil;
begin
  try
    Plist := AddNode(Plist, 1);
    Plist := AddNode(Plist, 2);
    Plist := AddNode(Plist, 3);
    Plist := AddNode(Plist, 4);
  finally
    WriteLn(ListToString(Plist, ''));
    FreeList(Plist);
  end;
end.


