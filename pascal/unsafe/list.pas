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
      FPNodeNext: TPNode;
  public
    constructor Create(Pitch: TPitch; PNode: TPNode);
  end;

constructor TNode.Create(Pitch: TPitch; PNode: TPNode);
begin
  FPitch := Pitch;
  FPNodeNext := PNode;
  WriteLn('Creating new node with pitch ' + FPitch.ToString);
end;

function Last(List: TPNode): TPNode;
begin
  if List^.FPNodeNext = nil then
  begin
    result := List;
  end
  else
  begin
    WriteLn('Last: going to next node');
    result := Last(List^.FPNodeNext);
  end;
end;

function AddNode(Head: TPNode; Pitch: TPitch): TPNode;
{ create a new node and add it to the beginning of a given list }
var
  NewNode: TNode;
  PNewNode: TPNode;
begin
  NewNode := nil;
  PNewNode := nil;

  try
    NewNode := TNode.Create(Pitch, Head);
    WriteLn('Added new node with pitch ' + NewNode.FPitch.ToString);
    PNewNode := @NewNode;
  finally
    result := PNewNode;
  end;
end;

procedure PrintList(Head: TPNode);
begin
  if Head = nil then
  begin 
    writeln();
  end
  else
  begin
    WriteLn('Trying to print list...');
    Write(Head^.FPitch.ToString + ' ');
    PrintList(Head^.FPNodeNext);
  end;
end;

procedure FreeList(Head: TPNode);
var
  PNextNode: TPNode;
begin
  if Head = nil then
  begin
    WriteLn('FreeList: reached end of list');
    WriteLn('FreeList: Freeing node...');
    FreeAndNil(Head);
    exit;
  end
  else
  begin
    WriteLn('FreeList: Freeing head.FPitch ...');
    FreeAndNil(Head^.FPitch);
    WriteLn('FreeList: Freeing next node...');
    PNextNode := Head^.FPNodeNext;
    FreeList(PNextNode);
  end;
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
    PrintList(PList);
    }

  finally
    FreeList(PList);
  end;
end.


