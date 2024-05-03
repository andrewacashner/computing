{$mode objfpc}{$H+}{$J-}

unit LinkedListMWI;

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
  end;

  TLinkedList = class
  private
    var
      FHead: TNode;
  
  public
    constructor Create(Items: Array of Integer);
    destructor Destroy(); override;
  end;

implementation

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
end;

{ TODO Still one block unfreed }
destructor TLinkedList.Destroy();
begin
  FreeAndNil(Self.FHead);
  inherited Destroy;
end;

end.
