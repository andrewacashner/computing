{$mode objfpc}{$H+}{$J-}
{$optimization tailrec}

{******************************** 
  Linked list with custom class
  AAC, 2021/10/06
*********************************}

program PitchList;

uses Sysutils;

{ CLASS: Name }
type 
  TName = class
  private
    var
      FFirstName: String;
      FLastName: String;
  public
    constructor Create(First, Last: String);
    function ToString: String; override;
  end;

constructor TName.Create(First, Last: String);
begin
  inherited Create();
  FFirstName := First;
  FLastName := Last;
end;

function TName.ToString: String;
begin
  result := FFirstName + ' ' + FLastName;
end;

{ LINKED LIST }
type
  TPNode = ^TNode;

  TNode = record
    FName: TName;
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
function AddNode(Head: TPNode; FirstName, LastName: String): TPNode;
var
  NewNode: TPNode;
  ThisNode: TPNode = nil;
begin
  new(NewNode);
  NewNode^.FName := TName.Create(FirstName, LastName);
  NewNode^.FPNext := nil;

  if Head = nil then
  begin
    Head := NewNode;
  end
  else
  begin
    ThisNode := Last(Head);
    ThisNode^.FPNext := NewNode;
  end;
  result := Head;
end;

procedure FreeList(Head: TPNode);
begin
  if Head <> nil then
  begin
    FreeList(Head^.FPNext);
    FreeAndNil(Head^.FName);
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
    Msg := Msg + Head^.FName.ToString;

    if Head^.FPNext <> nil then
    begin
      Msg := Msg + ', ';
    end
    else
    begin
      Msg := Msg + ' ';
    end;
    
    result := ListToString(Head^.FPNext, Msg);
  end;
end;

{ MAIN }
var
  PList: TPNode = nil;
begin
  try
    PList := AddNode(PList, 'Harry', 'Potter');
    PList := AddNode(PList, 'Hermione', 'Granger');
    PList := AddNode(PList, 'Ron', 'Weasley');
    PList := AddNode(PList, 'Draco', 'Malfoy');
    PList := AddNode(PList, 'Seamus', 'Finnegan');
    PList := AddNode(PList, 'Neville', 'Longbottom');
    PList := AddNode(PList, 'Luna', 'Lovegood');
    
  finally
    WriteLn(ListToString(Plist, ''));
    FreeList(PList);
  end;
end.


