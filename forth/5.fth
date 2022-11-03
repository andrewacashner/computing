\ Starting Forth, ch. 5, 2022/11/03

\ 1. -ab/c
: 5.1 ( a b c -- n )
\    >R * NEGATE R> / ;
    */ NEGATE ;

\ 2. Print largest value of these numbers: 6 70 123 45
: max4 ( n1 n2 n3 n4 -- max )
\    MAX >R MAX R> MAX ;
    MAX MAX MAX ;

\ 3. Convert temperatures between Celsius and Fahrenheit
\ ˚C = (˚F - 32) / 1.8
\ ˚F = (˚C x 1.8) + 32
\ ˚K = ˚C + 273
: f->c ( fahrenheit -- celsius )
    32 - 10 18 */ ;

: c->f ( celsius -- fahrenheit )
    18 10 */ 32 + ;

: k->c ( kelvin -- celsius )
    273 - ;

: c->k ( celsius -- kelvin )
    273 + ;

: f->k ( fahrenheit -- kelvin )
    f->c c->k ;

: k->f ( kelvin -- fahrenheit )
    k->c c->f ;
