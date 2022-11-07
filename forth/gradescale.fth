\ print a grading scale out of a given total
\
\ 100 A+
\  93 A
\  90 A-
\  87 B+
\  83 B
\  80 B-
\  77 C+
\  73 C
\  70 C-
\  67 D+
\  63 D
\  60 D-
\   0 E

: PERCENT_OF ( percent total -- result )
    SWAP 100 */
;

: GRADE_STEPS
    0
    60 
    63 
    67 
    70 
    73 
    77 
    80 
    83 
    87 
    90 
    93 
    100 
;

VARIABLE TOTAL_GRADE

: GRADESCALE ( total -- )
    TOTAL_GRADE !
    GRADE_STEPS
    BEGIN
        TOTAL_GRADE @ PERCENT_OF
        DUP CR 3 U.R
        0> WHILE
    REPEAT 
;
\ TODO use doubles for scores to one or two decimal points
\ TODO print letter names
