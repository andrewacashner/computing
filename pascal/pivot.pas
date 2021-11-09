{$mode objfpc}{$H+}{$J-}
{ @abstract(Take a list of a containing lists of b and return a list of b
  containing lists of a)

  @author(Andrew Cashner)
 
  2021/11/09 }
program Pivot;

uses SysUtils, Classes, Generics.Collections;

type
  TVoice = TStringList;

  TChorus = specialize TObjectList<TVoice>;

var
  Soprano, Alto, Tenor, Bass: TVoice;
  Choir: TChorus;
begin
  Soprano := TVoice.Create;
  Alto := TVoice.Create;
  Tenor := TVoice.Create;
  Bass := TVoice.Create;
  Choir := TChorus.Create;

  try
    Soprano.Add('g''4 g''4 g''4 g''4');
    Soprano.Add('g''1');

    Alto.Add('e''4 e''4 e''4 e''4');
    Alto.Add('e''1');

    Tenor.Add('c''4 c''4 c''4 c''4');
    Tenor.Add('c''1');

    Bass.Add('c4 c4 e4 g4');
    Bass.Add('c1');

    Choir.Add(Soprano);
    Choir.Add(Alto);
    Choir.Add(Tenor);
    Choir.Add(Bass);

  finally
    FreeAndNil(Choir);
  end;
end.


