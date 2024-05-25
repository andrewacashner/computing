#!/usr/bin/env python

import sys
import argparse
from enum import Enum, auto
import re
import itertools as it

parser = argparse.ArgumentParser(
        prog="interval",
        description="Calculate musical intervals",
        epilog="Copyright © 2024 Andrew A. Cashner")

parser.add_argument("pitch1")
parser.add_argument("pitch2")

"""
pure table approach?
[
        [PC_C, PC_D, PC_E, PC_F, PC_G, PC_A, PC_B],
        ['C', 'D', 'E', 'F', 'G', 'A', 'B'],
        [0, 2, 4, 5, 7, 9, 11]
]

str(enum): table[1][enum.value]
offset(enum): table[2][enum.value]
from_string(s): table[0][table[1].index(s)]
in_gamut(s): s in table[1]
diatonic_to_chromatic(n): table[2][n]
"""

class Gamut(Enum):
    PC_C = 0
    PC_D = 1
    PC_E = 2
    PC_F = 3
    PC_G = 4
    PC_A = 5
    PC_B = 6

    __LETTERS = ['C', 'D', 'E', 'F', 'G', 'A', 'B']
    __OFFSETS = [ 0,   2,   4,   5,   7,   9,  11 ]

    def __str__(self):
        return self.__LETTERS[self.value]

    def chromatic_offset(self):
        return self.__OFFSETS[self.value]

    @classmethod
    def from_string(cls, s):
        s = s.upper()
        if s in cls.__LETTERS:
            return cls(cls.__LETTERS.index(s))
        else:
            raise Exception('Bad pitch letter name input')

class Accidental(Enum):
    __OFFSET = -2

    DOUBLE_FLAT  = -2
    FLAT         = -1
    NATURAL      =  0
    SHARP        =  1
    DOUBLE_SHARP =  2

    __INPUTS =  ['bb', 'b', '', '#', '##']
    __STRINGS = ['𝄫', '♭', '♮', '♯', '𝄪']

    def __str__(self): 
        return self.__STRINGS[self.value - self.__OFFSET]

    @classmethod
    def from_string(cls, s):
        if s not in cls.__INPUTS:
            raise Exception('Bad accidental input')

        value = cls.__INPUTS.index(s) + cls.__OFFSET
        return cls(value)

class Imperfect_Quality(Enum):
    __OFFSET = -2

    DIMINISHED  = -2
    MINOR       = -1
    MAJOR       =  0
    AUGMENTED   =  1 

    __STRINGS = ['d', 'm', 'M', 'a']

    def __str__(self):
        return self.__STRINGS[self.value - self.__OFFSET]

    @classmethod
    def from_int(cls, n):
        return cls(n)

class Perfect_Quality(Enum):
    __OFFSET = -1

    DIMINISHED  = -1
    PERFECT     = 0 
    AUGMENTED   = 1

    __STRINGS = ['d', 'P', 'a']

    def __str__(self):
        return self.__STRINGS[self.value - self.__OFFSET]

    @classmethod
    def from_int(cls, n):
        return cls(n)

class Pitch:
    def __init__(self, pname, accid=Accidental.NATURAL):
        self.pname = pname
        self.accid = accid

    def __str__(self):
        return f"{self.pname}{self.accid}"

    @classmethod
    def from_string(cls, input_string):
        pattern = r"([a-zA-Z]{1})([b#]*)"
        parsed = re.match(pattern, input_string)
        if parsed:
            (letter_str, accid_str) = parsed.groups()
        else:
            raise Exception('Bad pitch input')

        pname = Gamut.from_string(letter_str)
        accid = Accidental.from_string(accid_str)
        return cls(pname, accid)

    def abs7(self):
        return self.pname.value

    def abs12(self):
        start = self.pname.chromatic_offset()
        return start + self.accid.value

class Interval:
    def __init__(self, base, quality):
        self.base = base
        self.quality = quality

    def __str__(self):
        return f"{self.quality}{self.base}"

    @staticmethod
    def is_perfect(interval):
        return interval in [0, 3, 4] # zero index

    @classmethod
    def diff7(cls, noteA, noteB):
        (valA, valB) = tuple([note.abs7() for note in [noteA, noteB]])
        return (valB - valA) % 7

    @classmethod
    def diff12(cls, noteA, noteB):
        (valA, valB) = tuple([note.abs12() for note in [noteA, noteB]])
        return (valB - valA) % 12
 
    @classmethod
    def from_pitches(cls, pitchA, pitchB):
        diatonic_interval = cls.diff7(pitchA, pitchB)
        offset = Gamut(diatonic_interval).chromatic_offset()
        
        chromatic_interval = cls.diff12(pitchA, pitchB)
        interval_diff = chromatic_interval - offset

        quality = Perfect_Quality \
                if Interval.is_perfect(diatonic_interval) \
                else Imperfect_Quality

        alteration = quality.from_int(interval_diff)
        
        # Output interval with 1-index (interval of 1 is a "second")
        return cls(diatonic_interval + 1, alteration)

    @classmethod
    def from_strings(cls, stringA, stringB):
        vals = [Pitch.from_string(s) for s in [stringA, stringB]]
        return cls.from_pitches(*vals)

# def melodic_intervals(pitches):
#     return [Interval.from_strings(*pair) for pair in it.pairwise(pitches)]

def main(args=parser.parse_args()):
    (pitch1_str, pitch2_str) = (args.pitch1, args.pitch2)
    (pitch1, pitch2) = tuple([Pitch.from_string(p) 
                              for p in [pitch1_str, pitch2_str]])
    interval = Interval.from_pitches(pitch1, pitch2)
    print(f"{pitch1} to {pitch2} = {interval}")

if __name__ == "__main__":
    sys.exit(main())
