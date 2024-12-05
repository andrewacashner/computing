#!/usr/bin/env python3

import sys
import argparse
import random

parser = argparse.ArgumentParser(
        prog="numGen",
        description=
        "Generate a list of random numbers from 0 to a given maximum")

parser.add_argument(
        "max",
        type=int,
        nargs="?",
        help="Maximum value",
        default=100)

def rand_list(high):
    return [random.randint(0, 100) for n in range(0, high)]

def list_to_string(ls):
    return " ".join([f"{n}" for n in ls])

def main(args=parser.parse_args()):
    randoms = rand_list(args.max)
    print(list_to_string(randoms))

if __name__ == "__main__":
    sys.exit(main())

