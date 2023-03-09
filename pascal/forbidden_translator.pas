{$mode objfpc}{$H+}{$J-}

program ForbiddenTranslator(input, output);

{ Translate a string to Joy's Forbidden Language. 
  By Joy and Daddy, 2022/02/27
}

uses SysUtils;

function SingleLetter(InputChar: Char): String;
var 
    ForbiddenString: String;
begin
    case UpCase(InputChar) of
        'A' : ForbiddenString := 'Si';
        'B' : ForbiddenString := 'Wa'; 
        'C' : ForbiddenString := 'La'; 
        'D' : ForbiddenString := 'Ni'; 
        'E' : ForbiddenString := 'Lu'; 
        'F' : ForbiddenString := 'Mi'; 
        'G' : ForbiddenString := 'Yi'; 
        'H' : ForbiddenString := 'He'; 
        'I' : ForbiddenString := 'In'; 
        'J' : ForbiddenString := 'Ge'; 
        'K' : ForbiddenString := 'Ki'; 
        'L' : ForbiddenString := 'Li'; 
        'M' : ForbiddenString := 'Ma'; 
        'N' : ForbiddenString := 'Nu'; 
        'O' : ForbiddenString := 'Ot'; 
        'P' : ForbiddenString := 'Pe'; 
        'Q' : ForbiddenString := 'Un'; 
        'R' : ForbiddenString := 'Ri'; 
        'S' : ForbiddenString := 'Swa';
        'T' : ForbiddenString := 'Ti'; 
        'U' : ForbiddenString := 'Uv'; 
        'V' : ForbiddenString := 'Vi'; 
        'W' : ForbiddenString := 'Wu'; 
        'X' : ForbiddenString := 'Ya'; 
        'Y' : ForbiddenString := 'Pu'; 
        'Z' : ForbiddenString := 'Zi';
    else
        ForbiddenString := InputChar;
    end;
    result := ForbiddenString;
end;

function DoubleLetters(InputStr: String): String;
var 
    ForbiddenString: String = '';
begin
    case UpperCase(InputStr) of
        'OO' : ForbiddenString := 'Sela';
        'OU' : ForbiddenString := 'Walu';
        'OW' : ForbiddenString := 'Sule';
        'OA' : ForbiddenString := 'Wavan';
        'AI' : ForbiddenString := 'Yeka';
        'EA' : ForbiddenString := 'Maya';
        'EE' : ForbiddenString := 'Aya';
        'TH' : ForbiddenString := 'Selu';
        'CH' : ForbiddenString := 'Melu';
        'SH' : ForbiddenString := 'Yaku';
    end;
    result := ForbiddenString;
end;

function Translate(InputStr: String): String;
const DoubleLetterSet: String = 'OAETCS';
var 
    LetterPosition: Integer;
    ThisLetter: Char;
    ThisDoubleLetter: String;
    ThisForbiddenString: String;
    Translation: String = '';
begin
    LetterPosition := 0;
    while LetterPosition < InputStr.Length do
    begin
        ThisForbiddenString := '';    
        ThisLetter := InputStr.Chars[LetterPosition];

        if DoubleLetterSet.Contains(UpCase(ThisLetter)) then
        begin
            ThisDoubleLetter := InputStr.Substring(LetterPosition, 2);
            ThisForbiddenString := DoubleLetters(ThisDoubleLetter);

            if ThisForbiddenString <> '' then
            begin
                Inc(LetterPosition);
            end;
        end;

        if ThisForbiddenString = '' then
        begin
            ThisForbiddenString := SingleLetter(ThisLetter);
        end;

        Translation := Translation + ThisForbiddenString;
        Inc(LetterPosition);
    end;
    result := Translation;
end;

var InputText, OutputText: String;
begin
    ReadLn(InputText);
    OutputText := Translate(InputText);
    WriteLn(OutputText);
end.

