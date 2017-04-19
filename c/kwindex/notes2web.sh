#!/usr/bin/env sh

# notes2web.sh -- Andrew Cashner, 2017/04/18

# USAGE
# sh notes2web.sh input output

# Take markdown files from given input directory and produce a tree of HTML
# files in given output directory including index of keywords

set -e

usage="USAGE: mdmake.sh input output"

# Ensure that there are two arguments for input and output directory
if [ $# -lt 2 ] ; then 
    echo "$usage" 
    exit 1
fi

inputDir="$1"
outputDir="$2"

# Clear output directory
if [ -f "$outputDir"/* ]; then 
    rm "$outputDir"/*
fi

# Make the index of keywords in markdown
./kwindex.cx -o "$outputDir"/kwindex.md "$inputDir"/*.md

# Convert the index of keywords to html
pandoc -s -o "$outputDir"/kwindex.html "$outputDir"/kwindex.md

# Convert all the markdown files and put them in the output directory
cp "$inputDir"/*.md "$outputDir"/

cd "$outputDir"
for file in *.md; do 
    # Add a link to the home page
    echo -e "\n[Home](./index.html)" >> "$file"

    # Convert it to html
    pandoc -s -o "${file%.md}".html "$file";
done


# Write an index file in markdown with links to the contents
echo "# Contents" > index.md
for file in *.html; do
    echo "- [${file%.html}]($file)" >> index.md
done

# Convert the contents file
pandoc -s -o index.html index.md

# Clean up
rm ./*.md


