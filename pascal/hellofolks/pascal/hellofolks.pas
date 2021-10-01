{ hellofolks - Say hello to a list of people
  Andrew Cashner, 2021/10/01
}

program hellofolks;

const
    Folks: Array of UnicodeString = ( 'Joseph'
                                    , 'José'
                                    , 'Josephine'
                                    , 'Giuseppe'
                                    , 'Iosephus'
                                    , 'Josefina'
                                    , 'Josephine'
                                    );
var
    Name: UnicodeString;

begin
    for Name in Folks do
        begin
            WriteLn('Hello, ' + Name + '!');
        end;
end.




