( spell out a number from -4 to 4 )
: speller.sign ( n -- )
    dup 0< if ." negative " then abs ;

: speller ( n -- )
    dup abs 4 > if ." Out of range" 
    else speller.sign
    dup 0= if ." zero" else
    dup 1 = if ." one" else
    dup 2 = if ." two" else
    dup 3 = if ." three" else
    ." four"
    then then then then then drop ;


