#!/bin/sh

bibfile="$1"

for file in *.txt;
do 
    langid="${file%.txt}"
    mapfile -t keys < "$file"
    for key in "${keys[@]}"; 
    do 
        sed -E "s/(@[^\{]*\{$key,)/\1\nLangid=\{$langid\},/g" "$bibfile" -i
    done
done
