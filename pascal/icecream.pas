{ Print different outputs depending on input
  From Pascal Wikibook
  2021/09/28 }
program iceCream(input, output);
var
    response: char;
begin
    writeLn('Do you like ice cream?');
    writeLn('Type "y" for "yes" or "n" for "no".');
    writeLn('Confirm your selection by pressing Enter.');

    readLn(input, response);
    if response = 'y' then
        writeLn('Awesome!')
    else
        writeLn('That''s a pity!');
end.

