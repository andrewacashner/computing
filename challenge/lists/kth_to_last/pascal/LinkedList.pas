{$mode objfpc}{$H+}{$J-}

{ 
  kth item from end of singly linked list
  2024/05/03
}

unit LinkedList;

interface

uses SysUtils; 

type
  TNode = class
  private
    var
      FData: Integer;
      FNext: TNode;
  public
    constructor Create(Data: Integer);
    destructor Destroy(); override;

    function ToString(): String; override;
  end;

  TLinkedList = class
  private
    var
      FHead: TNode;
  
  public
    constructor Create(Items: Array of Integer);
    destructor Destroy(); override;

    function ToString(): String; override;
    function Length(): Integer;
    function At(N: Integer): Integer;
  end;

implementation

{ LINKED LIST NODE }
constructor TNode.Create(Data: Integer); 
begin
  inherited Create();
  Self.FData := Data;
  Self.FNext := Nil;
end;

destructor TNode.Destroy();
begin
  FreeAndNil(Self.FNext);
  inherited Destroy();
end;

function TNode.ToString(): String;
begin
  result := IntToStr(Self.FData);
end;

{ LINKED LIST }
constructor TLinkedList.Create(Items: Array of Integer);
var
  Head, Last, ThisNode: TNode;
  ThisItem: Integer;
begin
  inherited Create();
  Head := Nil;
  for ThisItem in Items do
  begin
    ThisNode := TNode.Create(ThisItem);
    if not Assigned(Head) then
    begin
      Head := ThisNode;
      Last := Head;
    end
    else
    begin
      Last.FNext := ThisNode;
      Last := ThisNode;
    end;
  end;
  Self.FHead := Head;
  WriteLn('Created TLinkedList: ' + Self.ToString());
end;

destructor TLinkedList.Destroy();
begin
  FreeAndNil(Self.FHead);
  inherited Destroy;
end;

function TLinkedList.Length(): Integer;
var 
  Count: Integer = 1; 
  Current: TNode;
begin
  Current := Self.FHead;
  while Assigned(Current.FNext) do
  begin
    Inc(Count);
    Current := Current.FNext;
  end;
  result := Count;
end;

function TLinkedList.At(N: Integer): Integer;
var
  Current: TNode;
  ThisIndex: Integer = 0;
begin
  Current := Self.FHead;
  if N > Self.Length() then
    { TODO exception message not passed on? }
    raise Exception.Create('Index out of range');

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

function TLinkedList.ToString(): String;
var 
  Current: TNode;
  OutputMsg: String = '(';
begin
  Current := Self.FHead;
  OutputMsg := OutputMsg + Current.ToString();
  while Assigned(Current.FNext) do
  begin
    OutputMsg := OutputMsg + ', ' + Current.FNext.ToString();
    Current := Current.FNext;
  end;
  result := OutputMsg + ')';
end;

end.
