#!/bin/sh

SELECTION="$1"
TEXFILE="$2"

for file in "$SELECTION"
do 
    convert "$file" -scale 2700 -quality 50% "$file" 
    echo "\\includeimage{$file}" >> includes.tex 
done

pdflatex "$TEXFILE"


