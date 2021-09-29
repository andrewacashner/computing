{$mode objfpc}{$H+}{$J-}

{ 
  Check off four items from a to-do list
  Basic test of a class in Object Pascal
  Andrew Cashner, 2021/09/29
}

program ToDo;

type
    TToDoList = class
    private
        const
            Max = 4;
        var
            Counter: integer;
    public
        procedure Next;
        function AllDone: Boolean;
    end;

procedure TToDoList.Next;
begin
    Inc(Counter);
    WriteLn('One more task done!');
end;

function TToDoList.AllDone: Boolean;
begin
    Result := Counter >= Max
end;

var 
    TaskList : TToDoList;

begin
    TaskList := TToDoList.Create;
    
    WriteLn('Do one of the following: breathe, swallow, jump, or think.');
    WriteLn('Press ENTER when you complete one of these tasks.');

    repeat
        readln();
        TaskList.Next;
    until TaskList.AllDone = True;

    WriteLn('You did all the tasks!');
end.
