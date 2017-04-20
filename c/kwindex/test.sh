#!/usr/bin/env sh

# test.sh -- Andrew A. Cashner, 2017/04/20
# Test program for notes2web and kwindex

set -e

sh notes2web.sh test/input test/output
xdg-open test/output/index.html &

exit 0

