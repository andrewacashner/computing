#!/usr/bin/env python

# Of course there is a simpler way to do this automatically using numpy.rot90
#
# There is also probably a better way to print the array using numpy
# (array2string with a formatter?)

import sys
import argparse
import numpy as np

parser = argparse.ArgumentParser(
        prog="rotate",
        description="Rotate an NxN matrix 90 degrees",
        epilog="Copyright © 2024 Andrew A. Cashner")

parser.add_argument("filename")

def render_matrix(matrix, 
                  zero='□',             # U+25A1 (white square)
                  nonzero='■',          # U+25A0 (black square)
                  row_separator=' ',
                  col_separator='\n'):

    def join_formatted(separator, format_fn, source_list):
        return separator.join(map(format_fn, source_list))

    def render_line(line):
        return join_formatted(row_separator, 
                              lambda n: zero if n == 0 else nonzero, 
                              line)
    
    return join_formatted(col_separator, render_line, matrix.tolist())

""" One turn is 90 degrees """
def rotate_matrix(matrix, turns=1):
    (size, _) = matrix.shape
    last = size - 1
    rotated = np.zeros(matrix.shape)
    for (x, y), item in np.ndenumerate(matrix):
        match turns:
            case 1:
                coords = (y, last - x)
            case 2:
                coords = (last - x, last - y)
            case 3:
                coords = (last - y, x)
            case _:
                coords = (x, y)

        (new_x, new_y) = coords
        rotated[new_x][new_y] = item

    return rotated

def main(args=parser.parse_args()):
    matrix = np.genfromtxt(args.filename, dtype=None)
    rotations = '\n\n'.join([render_matrix(rotate_matrix(matrix, n)) 
                             for n in range(0, 5)])
    print(rotations)
    

if __name__ == "__main__":
    sys.exit(main())


