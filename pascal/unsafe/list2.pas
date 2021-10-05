{$mode objfpc}{$H+}{$J-}
{$optimization tailrec}

{******************************** 
  Linked list with custom class
  AAC, 2021/10/05
*********************************}

program PitchList;

uses Sysutils;

{ CLASS: Pitch }
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
  NewNode: TNode;
  ThisNode: TPNode = nil;
begin
  NewNode.FName := TName.Create(FirstName, LastName);
  NewNode.FPNext := nil;

  if Head = nil then
  begin
    Head := @NewNode;
  end
  else
  begin
    ThisNode := Last(Head);
    ThisNode^.FPNext := @NewNode;
  end;
  result := Head;
end;

procedure FreeList(Head: TPNode);
var
  PNextNode: TPNode = nil;
begin
  if Head <> nil then
  begin
    PNextNode := Head^.FPNext;
    FreeAndNil(Head^);
    FreeAndNil(Head^.FName);
    FreeList(PNextNode);
  end;
end;
{
function ListToString(Head: TPNode; Msg: String): String;
var
  PNextNode: TPNode = nil;
begin
  if Head = nil then
  begin 
    result := Msg;
  end
  else
  begin
    PNextNode := Head^.FPNext;
    Msg := Msg + Head^.FName.ToString + ' ' + ListToString(PNextNode, Msg);
  end;
end;
}
{ MAIN }
var
  { Names: String = ''; }
  PList: TPNode = nil;
begin
  try
    PList := AddNode(PList, 'Harry', 'Potter');
    {
    PList := AddNode(PList, 'Hermione', 'Granger');
    PList := AddNode(PList, 'Ron', 'Weasley');
    PList := AddNode(PList, TName.Create('Draco', 'Malfoy'));
    PList := AddNode(PList, TName.Create('Seamus', 'Finnegan'));
    PList := AddNode(PList, TName.Create('Neville', 'Longbottom'));
    PList := AddNode(PList, TName.Create('Luna', 'Lovegood'));
    }
    {
    Names := ListToString(PList, Names);
    }
  
  finally
    FreeList(PList);
  end;
end.


