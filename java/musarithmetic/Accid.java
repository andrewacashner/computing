package com.andrewcashner.musarithmetic;

import java.util.*;

enum Accid {
    TRI_FLAT    ("bbb", "𝄫♭",  "eseses", -3),
    DBL_FLAT    ("bb",  "𝄫",   "eses",   -2),
    FLAT        ("b",   "♭",   "es",     -1),
    NATURAL     ("",    "",    "",        0),
    SHARP       ("#",   "♯",   "is",      1),
    DBL_SHARP   ("##",  "𝄪",   "isis",    2),
    TRI_SHARP   ("###", "♯♯♯", "isisis",  3);

    private String input;
    private String outputUnicode;
    private String outputLy;
    private int adjustment;

    public static final Accid DEFAULT = Accid.NATURAL;

    private static final Map<String, Accid> lookupByInput = new HashMap<>();

    private static final Map<Integer, Accid> lookupByAdjustment
        = new HashMap<>();

    static {
        Arrays.stream(Accid.values())
            .forEach(a -> {
                    lookupByInput.put(a.input(), a);
                    lookupByAdjustment.put(a.adjustment(), a);
            });
    }

    private Accid(String input, String outputUnicode, String outputLy,
            int adjustment) {

        this.input = input;
        this.outputUnicode = outputUnicode;
        this.outputLy = outputLy;
        this.adjustment = adjustment;
    }

    public static Accid of(int adjustment) throws IllegalArgumentException {
        Accid match = Accid.lookupByAdjustment.get(adjustment);
        if (match == null) {
            throw new IllegalArgumentException(String.format(
                        "Invalid accidental value %d", adjustment));
        }
        return match;
    }

    public static Accid of(String input) throws IllegalArgumentException {
        Accid match = Accid.lookupByInput.get(input); 
        if (match == null) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized accidental input %s", input));
        }
        return match;
    }

    private String input() {
        return this.input;
    }

    public String outputUnicode() {
        return this.outputUnicode;
    }

    public String outputLy() {
        return this.outputLy;
    }

    public int adjustment() {
        return this.adjustment;
    }

    public String toString() {
        return this.outputUnicode();
    }

    public String toLy() {
        return this.outputLy();
    }
}
