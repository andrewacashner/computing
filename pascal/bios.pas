{$mode objfpc}{$H+}{$J-}
{***************************
  Binary tree
  AAC 2021/10/03
 ***************************}

program Tree;

uses Sysutils;

type
  TDateYMD = class
  private
    var
      FYear: Integer;
      FMonth: Integer;
      FDay: Integer;
  public
    constructor Create(Year, Month, Day: Integer);
    function ToString: String; override;
  end;

  TBio = class
  private
    var
      FFirstName: String;
      FMiddleName: String;
      FSurname: String;
      FSuffix: String;
      FBirthDate: TDateYMD;
      FDeathDate: TDateYMD;
  public
    constructor Create(First, Middle, Surname, Suffix: String; 
      Birth, Death: TDateYMD);
    function ToString: String; override;
  end;

constructor TDateYMD.Create(Year, Month, Day: Integer);
begin
  FYear := Year;
  FMonth := Month;
  FDay := Day;
end;

function TDateYMD.ToString: String;
begin
  Result := IntToStr(FYear) + '/' + IntToSTr(FMonth) + '/' + IntToStr(FDay);
end;

constructor TBio.Create(First, Middle, Surname, Suffix: String; 
  Birth, Death: TDateYMD);
begin
  FFirstName := First;
  FMiddleName := Middle;
  FSurname := Surname;
  FSuffix := Suffix;
  FBirthDate := Birth;
  FDeathDate := Death;
end;

function TBio.ToString: String;
var
  BasicName, FullSuffix, DateRange: String;
begin
  if length(FMiddleName) > 0 then
    begin
      BasicName := FFirstName + ' ' + FMiddleName + ' ' + FSurname;
    end
  else
    begin
      BasicName := FFirstName + ' ' + FSurname;
    end;

  if length(FSuffix) > 0 then
    begin
      FullSuffix := ', ' + FSuffix + ' ';
    end
  else
    begin
      FullSuffix := '';
    end;

  DateRange := '(' + FBirthDate.ToString + '-' + FDeathDate.ToString + ')';

  Result := BasicName + ' ' + FullSuffix + DateRange;
end;


{ MAIN }
var
  GutierrezDePadilla, Mozart: TBio;
  GdPBirth, GdPDeath, MozBirth, MozDeath: TDateYMD;
begin
  { FYI the month and day on these dates is made up }
  GdPBirth := TDateYMD.Create(1580, 10, 01);
  GdPDeath := TDateYMD.Create(1660, 03, 15);
  GutierrezDePadilla := TBio.Create('Juan', '', 'Gutiérrez de Padilla', '',
                          GdPBirth, GdPDeath);
  
  MozBirth := TDateYMD.Create(1756, 06, 06);
  MozDeath := TDateYMD.Create(1791, 11, 05);
  Mozart := TBio.Create('Johannes Chyrostomus', 'Wolfgang Gottlieb', 'Mozart', '',
              MozBirth, MozDeath);

  writeLn(GutierrezDePadilla.ToString);
  writeLn(Mozart.ToString);

  FreeAndNil(GutierrezDePadilla);
  FreeAndNil(GdPBirth);
  FreeAndNil(GdPDeath);
  FreeAndNil(Mozart);
  FreeAndNil(MozBirth);
  FreeAndNil(MozDeath);
end.


