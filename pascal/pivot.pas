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
  TMeasure = TStringList;

type
  TScore = class(specialize TObjectList<TMeasure>)
  public
    function ToString: String; override;
  end;

function TScore.ToString: String;
var 
  ThisMeasure: TMeasure;
  ThisString: String;
  OutputLines: TStringList;
  OutputStr: String = '';
  MeasureNum, LayerNum: Integer;
begin
  OutputLines := TStringList.Create;
  try
    OutputLines.Add('<score>');
    MeasureNum := 0;
    for ThisMeasure in Self do
    begin
      Inc(MeasureNum);
      OutputLines.Add('  <measure n="' + IntToStr(MeasureNum) + '">');
      LayerNum := 0;
      for ThisString in ThisMeasure do
      begin
        Inc(LayerNum);
        OutputLines.Add('    <layer n="' + IntToStr(LayerNum) + '">');
        OutputLines.Add('      ' + ThisString);
        OutputLines.Add('    </layer>');
      end;
      OutputLines.Add('  </measure>');
    end;
    OutputLines.Add('</score>');
    OutputStr := OutputLines.Text

  finally
    FreeAndNil(OutputLines);
    result := OutputStr;
  end;
end;

function VoicesToMeasures(Choir: TChorus; Score: TScore): TScore;
var 
  MeasureNum: Integer;
  ThisVoice: TVoice;
  NewMeasure: TMeasure;
begin
  Score.Clear;
  for MeasureNum := 0 to (Choir[0].Count - 1) do
  begin
    NewMeasure := TMeasure.Create;
    for ThisVoice in Choir do
      NewMeasure.Add(ThisVoice[MeasureNum]);
    Score.Add(NewMeasure);
  end;
  result := Score;
end;


var
  Soprano, Alto, Tenor, Bass, ThisVoice: TVoice;
  Choir: TChorus;
  Score: TScore;
begin
  Soprano := TVoice.Create;
  Alto := TVoice.Create;
  Tenor := TVoice.Create;
  Bass := TVoice.Create;
  Choir := TChorus.Create;
  Score := TScore.Create;

  try
    Soprano.Add('| g''4 g''4 g''4 g''4');
    Soprano.Add('| g''1');
    Soprano.Add('| g''1');

    Alto.Add('| e''4 e''4 e''4 e''4');
    Alto.Add('| e''1');
    Alto.Add('| d''1');

    Tenor.Add('| c''4 c''4 c''4 c''4');
    Tenor.Add('| c''1');
    Tenor.Add('| b1');

    Bass.Add('| c4 c4 e4 g4');
    Bass.Add('| c1');
    Bass.Add('| g,1');

    Choir.Add(Soprano);
    Choir.Add(Alto);
    Choir.Add(Tenor);
    Choir.Add(Bass);

    for ThisVoice in Choir do
      WriteLn(ThisVoice.Text);

    Score := VoicesToMeasures(Choir, Score);

    WriteLn(Score.ToString);

  finally
    FreeAndNil(Score);
    FreeAndNil(Choir);
  end;
end.


