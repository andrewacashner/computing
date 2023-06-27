program timesig;

uses SysUtils;

var 
  Msg: String = '';
  Beats, Beat: Integer;

begin
  if (ParamCount = 1) then
  begin
    Beats := StrToInt(ParamStr(1));
    if Beats > 0 then
    begin
      Msg := '[';
      for Beat := 1 to Beats - 1 do
      begin
        Msg := Msg + '1+';
      end;
      Msg := Msg + '1]/4';
    end;
  end;

  WriteLn(Msg);
end.
