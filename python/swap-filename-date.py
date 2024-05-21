#!/usr/bin/env python3
"""
2024/05/16

Given filenames in format 'name-2024_05_##.tex', 
rename to '2024_05_##-name.tex'
"""

import sys
import os
import re

def swap(basename):
    pattern = r"([^0-9]*)-(2024_05_[0-9]*)"
    return re.sub(pattern, r"\2-\1", basename)

def main():
    filenames = os.listdir('.')
    for name in filenames:
        if name.endswith('.tex'):
            basename = name[:-4]
            new_name = swap(basename)
            if new_name != basename:
                new_name += '.tex'
                os.rename(name, new_name)

if __name__ == "__main__":
    sys.exit(main())

