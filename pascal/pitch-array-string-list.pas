{$mode objfpc}{$H+}{$J-}

{******************************** 
  Array of Pitch class with string list for output
  AAC, 2021/10/06
*********************************}

program PitchArray;

uses Sysutils, Classes;

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

  TPitchArray = Array of TPitch;


constructor TPitch.Create(PCname: TPitchClassLabel; Oct: Integer;
                          Accid: TAccidentalLabel);
begin
  FPitchClassName := PCname;
  FOctave := Oct;
  FAccidental := Accid;
end;

function TPitch.PitchClassNameString : String;
var
  PitchNames: Array of String = ('C', 'D', 'E', 'F', 'G', 'A', 'B');
begin
  result := PitchNames[ord(FPitchClassName)];
end; 

function TPitch.AccidentalString : String;
var
  AccidentalNames: Array of String = ('♭', '♮', '♯');
begin
  result := AccidentalNames[ord(FAccidental)];
end;

function TPitch.PitchNumChromatic: Integer;
var 
  ChromaticPitch: Array of Integer = (0, 2, 4, 5, 7, 8, 11);
begin
  result := ChromaticPitch[Ord(FPitchClassName)];
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

{ MAIN }
var
  Pitches: TPitchArray = nil;
  Msg: TStringList = nil;
  I, Max: Integer;
begin
  Msg := TStringList.Create();
  Msg.LineBreak := ', ';
  Msg.SkipLastLineBreak := true;

  try
    Pitches := TPitchArray.Create(TPitch.Create(pkC, 4, akNa),
                                  TPitch.Create(pkD, 4, akNa),
                                  TPitch.Create(pkE, 4, akFl),
                                  TPitch.Create(pkF, 4, akNa),
                                  TPitch.Create(pkG, 4, akNa),
                                  TPitch.Create(pkA, 4, akFl),
                                  TPitch.Create(pkB, 4, akNa),
                                  TPitch.Create(pkC, 5, akNa));
    Max := length(Pitches);
    for I := 0 to Max - 1 do
    begin
      Msg.Add(Pitches[i].ToString);
      FreeAndNil(Pitches[i]);
    end;
  finally
    writeLn(Msg.Text);
    FreeAndNil(Msg);
  end;
end.


