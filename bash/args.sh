#!/bin/sh
args=("$@")
echo "Before: ${args[@]}"
newargs=("${args[@]/%/.ref}")
echo "After: ${newargs[@]}"
exit 0
