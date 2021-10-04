{$mode objfpc}{$H+}{$J-}
{******************************** 
  Linked list with Pitch class
  AAC, 2021/10/02
*********************************}

program PitchList;

uses Sysutils, Generics.Collections;;

{ CLASS: Pitch }
type 
  TPitchClassLabel = (PCc, PCd, PCe, PCf, PCg, PCa, PCb);
  TAccidentalLabel = (Fl = -1, Na, Sh);

  TPitch = class
  private
    var
      FPitchClassName: TPitchClassLabel;
      FAccidental: TAccidentalLabel;
      FOctave: integer;
    function PitchClassNameString: string;
    function AccidentalString: string;
    function PitchNumChromatic: integer;
  public
    constructor Create(PCname: TPitchClassLabel; Oct: integer;
                        Accid: TAccidentalLabel);
    function StandardPitch: integer; 
    function ToString: string; override;
  end;

  { linked list }
  TNodePtr = ^TNode;
  TNode = Record
    Pitch: TPitch;
    NodePtr: TNodePtr;
  end;

constructor TPitch.Create(PCname: TPitchClassLabel; Oct: integer;
                          Accid: TAccidentalLabel);
begin
  FPitchClassName := PCname;
  FOctave := Oct;
  FAccidental := Accid;
end;

function TPitch.PitchNumChromatic: integer;
var 
  ChromaticPitch: array of integer = (0, 2, 4, 5, 7, 8, 11);
begin
  result := ChromaticPitch[Ord(FPitchClassName)];
end;

function TPitch.PitchClassNameString : string;
var
  PitchNames: array of string = ('c', 'd', 'e', 'f', 'g', 'a', 'b');
begin
  result := PitchNames[ord(FPitchClassName)];
end; 

function TPitch.AccidentalString : string;
var
  AccidentalNames: array of string = ('b', '', '#');
begin
  result := AccidentalNames[ord(FAccidental)];
end;

function TPitch.StandardPitch: integer; 
begin 
  result := FOctave * 12 + PitchNumChromatic + ord(FAccidental); 
end;

function TPitch.ToString: string; 
begin
  result :=  PitchClassNameString + AccidentalString + IntToStr(FOctave);
end;

{ LINKED LIST }
function Last(List: TNodePtr): TNodePtr;
begin
  if List^.NodePtr <> nil then
  begin
    List := Last(List^.NodePtr);
  end;
  result := List;
end;

function AddNode(Head: TNodePtr; Pitch: TPitch): TNodePtr;
var
  ThisNodePtr, NewNodePtr: TNodePtr;
begin
  new(NewNodePtr);
  NewNodePtr^.Pitch := Pitch;
  NewNodePtr^.NodePtr := nil;

  if Head = nil then
  begin
    Head  := NewNodePtr;
  end
  else
  begin
    ThisNodePtr := Last(Head);
    ThisNodePtr^.NodePtr := NewNodePtr;
  end;

  result := Head;
end;

procedure PrintList(Head: TNodePtr);
begin
  if Head <> nil then
  begin
    write(Head^.Pitch.ToString + ' ');
    PrintList(Head^.NodePtr);
  end
  else
  begin
    writeLn();
  end;
end;

procedure FreeList(Head: TNodePtr);
begin
  if Head <> nil then
  begin
    while Head^.NodePtr <> nil do
    begin
      FreeList(Head^.NodePtr);
      dispose(Head);
    end;
  end;
end;

{ Methods for Pitches }
function ChromaticInterval(Pitch1, Pitch2: TPitch): Integer;
begin
  result := Pitch1.StandardPitch - Pitch2.StandardPitch
end;

function IsTritone(Pitch1, Pitch2: TPitch): Boolean;
begin
  result := ChromaticInterval(Pitch1, Pitch2) = 6;
end;

{ MAIN }
var
  List: TNodePtr = nil;
  Pitch1, Pitch2: TPitch;
begin
  Pitch1 := TPitch.Create(PCc, 4, Sh);
  Pitch2 := TPitch.Create(PCg, 3, Na);
 
  List := AddNode(List, Pitch1);
  List := AddNode(List, Pitch2);
  List := AddNode(List, Pitch1);
  List := AddNode(List, Pitch1);
  List := AddNode(List, Pitch2);
  List := AddNode(List, Pitch1);

  PrintList(List);
  FreeList(List);
end.


