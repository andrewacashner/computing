: FISH ( -- )
    5 1 DO CR I 
        DUP 3 = IF ." Red " ELSE
        DUP 4 = IF ." Blue " ELSE
        . THEN THEN 
        ." fish" LOOP
;
