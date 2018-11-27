#!/bin/sh

today=$(date -I)
output=posts/"$today".md

if [ ! -f "$output" ]; then
    touch "$output"
    printf -- "--------------\n\n*%s*\n\n\n" "$today" > "$output"
    vim + "$output"
else
    echo "File "$output" already exists."
fi

