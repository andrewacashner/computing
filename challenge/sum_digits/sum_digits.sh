#!/usr/bin/env bash

sum=0
for n in {0..9}
do
    sum=$(($sum + 5 * (9 + 2 * $n)))   
done
echo $(($sum + 1))

