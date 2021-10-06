{$mode objfpc}{$H+}{$J-}

{******************************** 
  List of names with custom class
  AAC, 2021/10/06
*********************************}

program PitchList;

uses Sysutils, Classes, Generics.Collections;

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

{ LIST }
type
  TNameList = specialize TObjectList<TName>;

procedure AddName(Names: TNameList; FirstName, LastName: String);
begin
  if Names <> nil then
  begin
    Names.Add(TName.Create(FirstName, LastName));
  end;
end;

function NameListCommaString(Names: TNameList): String;
var
  Name: TName;
  NameStrings: TStringList;
  Msg: String;
begin
  NameStrings := TStringList.Create();
  NameStrings.LineBreak := ', ';
  NameStrings.SkipLastLineBreak := true;

  try
    for Name in Names do
    begin
      NameStrings.Add(Name.ToString);
    end;
  finally
    Msg := NameStrings.Text;
    FreeAndNil(NameStrings);
    result := Msg;
  end;
end;

{ MAIN }
var
  Names: TNameList;
begin
  Names := TNameList.Create();

  try
    AddName(Names, 'Harry', 'Potter');
    AddName(Names, 'Hermione', 'Granger');
    AddName(Names, 'Ron', 'Weasley');
    AddName(Names, 'Draco', 'Malfoy');
    AddName(Names, 'Seamus', 'Finnegan');
    AddName(Names, 'Neville', 'Longbottom');
    AddName(Names, 'Luna', 'Lovegood');
   
  finally
    WriteLn(NameListCommaString(Names));
    FreeAndNil(Names);
  end;
end.


