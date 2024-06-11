#!/usr/bin/env python

import sys
import argparse

parser = argparse.ArgumentParser(
        prog='binary',
        description='Manually convert an integer to binary',
        epilog='Copyright © 2024 Andrew A. Cashner')
parser.add_argument('integer')

def to_binary(given):
    binary_values = []
    quot = int(given)
    while quot > 0:
        (quot, rem) = divmod(quot, 2)
        binary_values.append(rem)

    return ''.join(['1' if v == 1 else '0' 
                      for v in reversed(binary_values)])

def main(args=parser.parse_args()):
    print(to_binary(args.integer))

if __name__ == '__main__':
    sys.exit(main())



