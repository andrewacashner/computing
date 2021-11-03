{ `ListAsTree`

  Implement a tree as a nested list

  Andrew Cashner, 2021/11/03
}

{$mode objfpc}{$H+}{$J-}

program ListAsTree;

uses SysUtils, Classes, Generics.Collections;

type 
  TBio = class
  private
    var
      FFirstName, FLastName: String;
      FBirthDate, FDeathDate: Integer;
  public
    constructor Create(First, Last: String; Birth, Death: Integer);
    function DateString: String;
    function ToString: String; override;
  end;

constructor TBio.Create(First, Last: String; Birth, Death: Integer);
begin
  FFirstName := First;
  FLastName := Last;
  FBirthDate := Birth;
  FDeathDate := Death;
end;

function TBio.DateString: String;

function SingleDateToString(Date: Integer): String;
begin
  if Date = -1 then
    result := '*'
  else
    result := IntToStr(FBirthDate);
end;

begin
  result := '(' + SingleDateToString(FBirthDate) + '-' +
            SingleDateToString(FDeathDate) + ')'; 
end;

function TBio.ToString: String;
begin
  result := FFirstName + ' ' + FLastName + ' ' + Self.DateString
end;

type
  TSiblings = specialize TObjectList<TBio>;
  TFamilyTree = specialize TObjectList<TSiblings>;

{ MAIN }
var
  Generation: TSiblings;
  Cashners: TFamilyTree;
  ThisBio: TBio;
  ThisGeneration: TSiblings;
begin
  Generation := TSiblings.Create;
  Cashners := TFamilyTree.Create;
  try
    Generation.Add(TBio.Create('Matthew', 'Cashner', 1978, -1));
    Generation.Add(TBio.Create('Andrew', 'Cashner', 1981, -1));
    Cashners.Add(Generation);

    for ThisGeneration in Cashners do
      for ThisBio in ThisGeneration do
        WriteLn(ThisBio.ToString);

  finally
    FreeAndNil(Cashners);
  end;
end.
