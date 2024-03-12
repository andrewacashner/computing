#!/usr/bin/env python

import sys
import argparse

parser = argparse.ArgumentParser(
        prog="Square",
        description="Print a square with the given text moving around the edges clockwise",
        epilog="Copyright (c) 2024 Andrew A. Cashner")

parser.add_argument(
        "msg", 
        metavar="MSG",
        type=str,
        help="The message to print")

parser.add_argument(
        "-c",
        "--counterclockwise",
        help="Move counterclockwise",
        action="store_true")

# Given an input string, print out a square of text where the string 
# is repeated on each side of the square, proceeding clockwise around 
# the edge. The right edge will read top to bottom, the bottom edge 
# right to left, the left edge bottom to top.
# Space out the horizontals to make something closer to a square.
def square(s, counterclockwise=False):
    def spread(s):
        return ' '.join(list(s))
    
    def middle(left, right):
        gap = ' ' * (len(spread(left)) - 2)
        return [l + gap + r for l, r in zip(left, right)] 

    def horizontals(s, counterclockwise=False):
        return (s[::-1], s) if counterclockwise else (s, s[::-1])

    (top, bottom) = horizontals(s, counterclockwise)
    sides = middle(bottom, top)

    return '\n'.join([spread(top), *sides, spread(bottom)])


def main(args=parser.parse_args()):
    output = square(args.msg, args.counterclockwise)
    print(output)

if __name__ == "__main__":
    sys.exit(main())
