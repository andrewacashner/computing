( guessing game 2022/11/02 )
: guess ( answer test -- )
    2dup = if ." correct!" 2drop exit else
    over > if ." too high" else
    ." too low"
    then then ;

    
