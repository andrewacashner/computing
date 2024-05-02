{$mode objfpc}{$H+}{$J-}

{ TODO Memory leak on deletion: see below }

unit LinkedList;

interface

uses SysUtils; 

type
  TLinkedList = class
  private
    var
      FData: Integer;
      FNext: TLinkedList;
  
  public
    constructor Create();
    constructor Create(Item: Integer);
    destructor Destroy(); override;

    class function CreateList(Items: Array of Integer): TLinkedList; static;
    function NodeToString(): String;
    function ToString(): String; override;
    function Length(): Integer;
    function At(N: Integer): Integer;
  end;

implementation

constructor TLinkedList.Create();
begin
  inherited Create;
end;

constructor TLinkedList.Create(Item: Integer);
begin
  inherited Create;
  Self.FData := Item;
  Self.FNext := Nil;
end;

class function TLinkedList.CreateList(Items: Array of Integer): TLinkedList;
var
  Node, Prev: TLinkedList;
  Head: TLinkedList = Nil;
  Item: Integer;
begin
  for Item in Items do
  begin
    Node := TLinkedList.Create(Item);
    if not Assigned(Head) then
    begin
      Head := Node;
      Prev := Node;
    end
    else
    begin
      Prev.FNext := Node;
      Prev := Node;
    end;
  end;
  result := Head;
end;

{ TODO Still one block unfreed }
destructor TLinkedList.Destroy();
begin
  if Assigned(Self.FNext) then
    Self.FNext.Destroy();
    
  inherited Destroy();
end;

function TLinkedList.Length(): Integer;
var 
  Count: Integer = 1; 
  Current: TLinkedList;
begin
  Current := Self;
  while Assigned(Current.FNext) do
  begin
    Inc(Count);
    Current := Current.FNext;
  end;
  result := Count;
end;

function TLinkedList.At(N: Integer): Integer;
var
  Current: TLinkedList;
  ThisIndex: Integer = 0;
begin
  Current := Self;
  if N > Self.Length() then
    raise Exception.create('Index out of range');

  if N < 0 then
    N := N + Self.Length();

  begin
    while (Assigned(Current) and (ThisIndex < N)) do
    begin
      Inc(ThisIndex);
      Current := Current.FNext;
    end;
    result := Current.FData;
  end;
end;

function TLinkedList.NodeToString(): String;
begin
  result := IntToStr(Self.FData);
end;

function TLinkedList.ToString(): String;
var 
  Current: TLinkedList;
  OutputMsg: String = '(';
begin
  Current := Self;
  OutputMsg := OutputMsg + Current.NodeToString();
  while Assigned(Current.FNext) do
  begin
    OutputMsg := OutputMsg + ', ' + Current.FNext.NodeToString();
    Current := Current.FNext;
  end;
  result := OutputMsg + ')';
end;

end.
