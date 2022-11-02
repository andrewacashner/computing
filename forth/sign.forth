: tf? if ." true" else ." false" then ;
: non-zero? 0= invert ;
: positive? 0 > ;
: negative? 0 < ;

( test value of integer )
: sign-test ( n -- )
    dup positive? if ." positive" else 
    dup negative? if ." negative" else
    dup 0=        if ." zero" 
    then then then drop ;

: rot.back ( a b c -- c a b )
    rot rot ;

: within ( n lo-limit hi-limit -- flag ) 
    ( rot dup rot < rot.back < and ; ) ( my version without rstack )
    over - >r - r> u< ; ( official implementation )

    (n - lo) <= (hi - lo)

(    n lo hi                            )
(    over    -- n lo hi lo              )
(    -       -- n lo (hi - lo)          )
(    >r      -- n lo | (hi - lo)        )
(    -       -- (n - lo)                )
(    r>      -- (n - lo) (hi - lo)      )
(    u<      -- (n - lo) < (hi - lo)    )



