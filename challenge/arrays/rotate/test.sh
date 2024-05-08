#!/usr/bin/env bash

if [ -f test.log ]; then
    rm test.log
fi

for program in ./bin/* 
do
    for i in input/*.txt 
    do
        echo "$program $i" >> test.log
        { time "$program" "$i" &> /dev/null ; } 2>> test.log
        echo "" >> test.log
    done
done
    

