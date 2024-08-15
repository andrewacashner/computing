#!/usr/bin/env python3

# 2024/08/15
# NB assuming well-formed input

import sys
import csv
import argparse
import random

parser = argparse.ArgumentParser(
        prog="namegroup",
        description="Given a CSV of names with lastname and firstname columns, merge the columns into one and sort them randomly into two groups",
        epilog="Copyright © 2024 Andrew A. Cashner")

parser.add_argument("filename")

def merge_names(csv):
    return [' '.join(row) for row in csv]

def random_group(ls):
    shuffled = random.shuffle(ls)
    midpoint = len(ls) // 2
    return (ls[0:midpoint], ls[midpoint + 1:])

def table_string(ls):
    groups = ['\n'.join(line) for line in ls]
    return '\n\n'.join(groups)

def main(args=parser.parse_args()):
    with open(args.filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile)
        groups = random_group(merge_names(csvreader))
        print(table_string(groups))

if __name__ == "__main__":
    sys.exit(main())
