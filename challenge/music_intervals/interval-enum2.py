from enum import Enum
import re
import itertools as it


"""
TODO 
- use aenum module to associate strings with enums, avoid [enum]_STR
  methods
- evaluate which class diff methods should be in
- use separate accidental class including all enum-based logic?
"""

GAMUT = [ 'C', 'D', 'E', 'F', 'G', 'A', 'B' ]

class Accidental:
    def __init__(self, value, input_str, output_str):
        self.value = value
        self.input_str = input_str
        self.output_str = output_str

    def __str__(self):
        return self.output_str
    
    DOUBLE_FLAT  = Accidental(-2, 'bb', '♭♭')
    FLAT         = Accidental(-1, 'b', '♭')
    NATURAL      = Accidental(0, '', '')
    SHARP        = Accidental(1, '#', '♯')
    DOUBLE_SHARP = Accidental(2, '##', '♯♯')

    @staticmethod
    def is_valid_input(s):
        return s in ['bb', 'b', '', '#', '##']

    @classmethod
    def from_string(cls, s):
        match s:
            case 'bb':
                return DOUBLE_FLAT
            case 'b':
                return FLAT
            case '#':
                return SHARP
            case '##':
                return DOUBLE_SHARP
            case _:
                return NATURAL


class Pitch:
    def __init__(self, pname, accid=Accidental.NATURAL):
        self.pname = pname
        self.accid = accid

    def __str__(self):
        return f"{self.pname}{str(self.accid)}"

    @classmethod
    def fromString(cls, input_string):
        pattern = r"([a-zA-Z]{1})([b#]*)"
        parsed = re.match(pattern, input_string)
        if parsed:
            (letter, accid_str) = parsed.groups()
        else:
            raise Exception("Bad pitch input")

        if letter.upper() not in GAMUT:
            raise Exception("Bad pitch letter name")

        if accid_str and not Accidental.is_valid_input(accid_str):
            raise Exception("Bad accidental input")

        accid = Accidental.from_string(accid_str)
        return cls(letter.upper(), accid)

    def abs7(self):
        return GAMUT.index(self.pname)

    def abs12(self):
        start = Interval.CHROMATIC_OFFSET[self.pname]
        return start + self.accid.value



class Interval:
    def __init__(self, base, quality):
        self.base = base
        self.quality = quality

    def __str__(self):
        qual_str = self.QUALITY_STRING[self.quality]
        return f"{qual_str}{self.base}"

    Quality = Enum('Quality', ['DIM', 'MIN', 'PERF', 'MAJ', 'AUG'])

    QUALITY_STRING = {
            Quality.DIM: 'd',
            Quality.MIN: 'm',
            Quality.PERF: 'P',
            Quality.MAJ: 'M',
            Quality.AUG: 'a'
            }

    CHROMATIC_OFFSET = {
            'C': 0,
            'D': 2,
            'E': 4,
            'F': 5,
            'G': 7,
            'A': 9,
            'B': 11
            }

    PERFECT_INTERVALS_ONE_INDEX = [1, 4, 5]

    ALTERATIONS = {
            'perfect': 
            { 
             Quality.DIM: -1, 
             Quality.PERF: 0, 
             Quality.AUG: 1 
             },
            'imperfect': 
            { 
             Quality.DIM: -2,
             Quality.MIN: -1,
             Quality.MAJ: 0,
             Quality.AUG: 1
             }
            }

    @classmethod
    def diff7(cls, noteA, noteB):
        (valA, valB) = tuple([note.abs7() for note in [noteA, noteB]])
        return (valB - valA) % 7

    @classmethod
    def diff12(cls, noteA, noteB):
        (valA, valB) = tuple([note.abs12() for note in [noteA, noteB]])
        return (valB - valA) % 12
 
    @classmethod
    def fromPitches(cls, pitchA, pitchB):
        diatonic_interval = cls.diff7(pitchA, pitchB)
        chromatic_interval = cls.diff12(pitchA, pitchB)

        category = 'perfect' if (diatonic_interval + 1) \
                in cls.PERFECT_INTERVALS_ONE_INDEX \
                else 'imperfect'

        offset = cls.CHROMATIC_OFFSET[GAMUT[diatonic_interval]]
        interval_diff = chromatic_interval - offset

        alterations = [key for (key, value) \
                in cls.ALTERATIONS[category].items() \
                if value == interval_diff]

        if len(alterations) > 0:
            alteration = alterations[0] 
        else:
            raise Exception("Bad quality")

        return cls(diatonic_interval + 1, alteration)

    @classmethod
    def fromStrings(cls, stringA, stringB):
        vals = [Pitch.fromString(s) for s in [stringA, stringB]]
        return cls.fromPitches(*vals)

def melodic_intervals(pitches):
    return [Interval.fromStrings(*pair) for pair in it.pairwise(pitches)]
