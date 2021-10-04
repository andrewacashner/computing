{$mode objfpc}{$H+}{$J-}
{
  Using generic linked lists with  a custom Pitch class
  AAC, 2021/10/02
}

program PitchList;

uses Sysutils, Classes, Generics.Collections;

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
  inherited Create();
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
  AccidentalAdjustment: Integer;
begin 
  AccidentalAdjustment := ord(FAccidental) - 1; // so Fl is -1 and Sh is +1
  result := FOctave * 12 + PitchNumChromatic + AccidentalAdjustment;
end;

function TPitch.ToString: String; 
begin
  result :=  PitchClassNameString + AccidentalString + IntToStr(FOctave);
end;

{
(* Methods for Pitches *)
function ChromaticInterval(Pitch1, Pitch2: TPitch): Integer;
begin
  result := Pitch1.StandardPitch - Pitch2.StandardPitch
end;

function IsTritone(Pitch1, Pitch2: TPitch): Boolean;
begin
  result := ChromaticInterval(Pitch1, Pitch2) = 6;
end;
}

{ LISTS }
type
  TPitchList = specialize TObjectList<TPitch>;
  TStdPitchList = specialize TList<Integer>;

{ MAIN }

var
  Pitches: TPitchList;
  StdPitches: TStdPitchList;
  MusicStrings, MusicNumStrings: TStringList;
  ThisPitch: TPitch;
  ThisPitchNum: Integer;
  
begin
  Pitches := TPitchList.Create();
  StdPitches := TStdPitchList.Create();
  MusicStrings := TStringList.Create();
  MusicNumStrings := TStringList.Create();

  MusicStrings.LineBreak := ', ';
  MusicNumStrings.LineBreak := MusicStrings.LineBreak;
  MusicStrings.SkipLastLineBreak := true;
  MusicNumStrings.SkipLastLineBreak := true;

  try
    Pitches.Add(TPitch.Create(pkC, 4, akSh));
    Pitches.Add(TPitch.Create(pkG, 3, akFl));
    Pitches.Add(TPitch.Create(pkC, 4, akSh));
    Pitches.Add(TPitch.Create(pkC, 4, akSh));
    Pitches.Add(TPitch.Create(pkG, 3, akFl));
    Pitches.Add(TPitch.Create(pkC, 4, akSh));

    for ThisPitch in Pitches do
    begin
      MusicStrings.Add(ThisPitch.ToString);

      ThisPitchNum := ThisPitch.StandardPitch;
      StdPitches.Add(ThisPitchNum);
      MusicNumStrings.Add(IntToStr(ThisPitchNum));
    end;
    
  finally
    WriteLn(MusicStrings.Text);
    WriteLn(MusicNumStrings.Text);

    FreeAndNil(Pitches);
    FreeAndNil(StdPitches);
    FreeAndNil(MusicStrings);
    FreeAndNil(MusicNumStrings);
  end;
end.


