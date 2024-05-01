#! /usr/bin/env sh

m4 -I library/songs "$1" | pandoc -o "${1%.md}.odp"
