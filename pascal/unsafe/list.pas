{$mode objfpc}{$H+}{$J-}
{******************************** 
  Linked list with Pitch class
  AAC, 2021/10/02
*********************************}

program PitchList;

uses Sysutils;

{ CLASS: Pitch }
type 
  TPitchClassLabel = (pkC, pkD, pkE, pkF, pkG, pkA, pkB);
  TAccidentalLabel = (akFl, akNa, akSh);

  TPitch = class
  private
    var
      FPitchClassName: TPitchClassLabel;
      FAccidental: TAccidentalLabel;
      FOctave: Integer;
    function PitchClassNameString: String;
    function AccidentalString: String;
    function PitchNumChromatic: Integer;
  public
    constructor Create(PCname: TPitchClassLabel; Oct: Integer;
                        Accid: TAccidentalLabel);
    function StandardPitch: Integer; 
    function ToString: String; override;
  end;

constructor TPitch.Create(PCname: TPitchClassLabel; Oct: Integer;
                          Accid: TAccidentalLabel);
begin
  FPitchClassName := PCname;
  FOctave := Oct;
  FAccidental := Accid;
end;

function TPitch.PitchNumChromatic: Integer;
var 
  ChromaticPitch: Array of Integer = (0, 2, 4, 5, 7, 8, 11);
begin
  result := ChromaticPitch[Ord(FPitchClassName)];
end;

function TPitch.PitchClassNameString : String;
var
  PitchNames: Array of String = ('c', 'd', 'e', 'f', 'g', 'a', 'b');
begin
  result := PitchNames[ord(FPitchClassName)];
end; 

function TPitch.AccidentalString : String;
var
  AccidentalNames: Array of String = ('b', '', '#');
begin
  result := AccidentalNames[ord(FAccidental)];
end;

function TPitch.StandardPitch: Integer; 
var 
  AccidentalAdjust: Integer;
begin 
  AccidentalAdjust := ord(Faccidental) - 1; // so Fl is -1 and Sh is +1
  result := FOctave * 12 + PitchNumChromatic + AccidentalAdjust;
end;

function TPitch.ToString: String; 
begin
  result :=  PitchClassNameString + AccidentalString + IntToStr(FOctave);
end;

{ LINKED LIST }
type
  TPNode = ^TNode;

  TNode = class
  private
    var
      FPitch: TPitch;
      FPNodePrev: TPNode;
      FPNodeNext: TPNode;
  public
    constructor Create(Pitch: TPitch; PNodePrev, PNodeNext: TPNode);
  end;

constructor TNode.Create(Pitch: TPitch; PNodePrev, PNodeNext: TPNode);
begin
  FPitch := Pitch;
  FPNodePrev := PNodePrev;
  FPNodeNext := PNodeNext;
end;

function Last(List: TPNode): TPNode;
begin
  try
    if List^.FPNodeNext <> nil then
    begin
      WriteLn('Last: going to next node');
      List := Last(List^.FPNodeNext);
    end;
  finally
    result := List;
  end;
end;

function AddNode(Head: TPNode; Pitch: TPitch): TPNode;
var
  NewNode: TNode;
  PThisNode: TPNode;
begin
  NewNode := nil;
  PThisNode := nil;

  try
    NewNode.Create(Pitch, nil, nil);

    if Head = nil then
    begin
      Head := @NewNode;
      WriteLn('Started new list with pitch ' + Pitch.ToString);
    end
    else
    begin
      PThisNode := Last(Head);
      NewNode.FPNodePrev := PThisNode;
      PThisNode^.FPNodeNext := @NewNode;
      WriteLn('Added pitch to end of list');
    end;
  finally
    result := Head;
  end;
end;

procedure PrintList(Head: TPNode);
begin
  if Head <> nil then
  begin
    WriteLn('Trying to print list...');
    write(Head^.FPitch.ToString + ' ');
    PrintList(Head^.FPNodeNext);
  end
  else
  begin
    writeLn();
  end;
end;

procedure FreeList(Head: TPNode);
var
  ThisNode: TPNode;
begin
  ThisNode := Last(Head);
  while ThisNode^.FPNodePrev <> nil do
  begin
    ThisNode := ThisNode^.FPNodePrev;
    FreeAndNil(ThisNode^.FPNodeNext^.FPitch);
    FreeAndNil(ThisNode^.FPNodeNext^);
  end;
  FreeAndNil(ThisNode^.FPitch);
  FreeAndNil(ThisNode);
end;

{ MAIN }
var
  PList: TPNode; 
begin
  PList := nil;

  try
    PList := AddNode(PList, TPitch.Create(pkC, 4, akSh));
    {
    PList := AddNode(PList, TPitch.Create(pkG, 3, akFl));
    PList := AddNode(PList, TPitch.Create(pkC, 4, akSh));
    PList := AddNode(PList, TPitch.Create(pkC, 4, akSh));
    PList := AddNode(PList, TPitch.Create(pkG, 3, akFl));
    PList := AddNode(PList, TPitch.Create(pkC, 4, akSh));
    }

    PrintList(PList);

  finally
    FreeList(PList);
  end;
end.


