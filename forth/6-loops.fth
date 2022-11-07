: RECTANGLE 
    256 0 DO 
        I 16 MOD 0= IF 
            CR THEN 
        ." *" 
    LOOP
;

: PENTAJUMPS
    50 0 DO
        I .
    5 +LOOP
;

: DOUBLING
    32767 1 DO
        I .
    I +LOOP
;

\ percent rounded up
: R% ( n1 -- n2 )
    10 */ 5 + 10 /
;

: COMPOUND ( amount interest -- )
    CR SWAP 21 1 DO
        ." YEAR " I . 3 SPACES
        2DUP R% +
        DUP ." BALANCE " . CR
    LOOP 2DROP
;

VARIABLE TARGET
: DOUBLED ( years rate amount -- )
    DUP 2* TARGET !
    ROT 1 DO
        CR ." year " I 2 U.R 
        2DUP R% +   \ add interest to principal
        DUP ."    balance " 5 U.R
        DUP TARGET @ > IF    
            CR CR ." more than doubled in "
            I . ." years " 
            LEAVE 
        THEN
    LOOP 2DROP 
;

