package com.andrewcashner.musarithmetic;

import java.util.*;

/**
 * An Accid represents an accidental, which represents a chromatic 
 * adjustment of a base pitch.
 */
public enum Accid implements PitchComponent {
    /** Triple flat */
    TRI_FLAT    ("bbb", "♭♭♭",  "eseses", -3),
    /** Double flat */
    DBL_FLAT    ("bb",  "𝄫",   "eses",   -2),
    /** Flat */
    FLAT        ("b",   "♭",   "es",     -1),
    /** Natural */
    NATURAL     ("",    "♮",    "",       0),
    /** Sharp */
    SHARP       ("#",   "♯",   "is",      1),
    /** Double sharp */
    DBL_SHARP   ("##",  "𝄪",   "isis",    2),
    /** Triple sharp */
    TRI_SHARP   ("###", "♯♯♯", "isisis",  3);

    private String input;
    private String outputUnicode;
    private String outputLy;
    private int adjustment;

    /** Default accidental is a natural. */
    public static final Accid DEFAULT = Accid.NATURAL;

    // Enable lookups by input or adjustment value.
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

    /**
     * Factory method that looks up Accid enum instance by its adjustment
     * value.
     *
     * @param adjustment Integer adjustment value
     * @return Accid enum
     * @throws IllegalArgumentException if value not in range
     */
    public static Accid of(int adjustment) throws IllegalArgumentException {
        Accid match = Accid.lookupByAdjustment.get(adjustment);
        if (match == null) {
            throw new IllegalArgumentException(String.format(
                        "Invalid accidental value %d", adjustment));
        }
        return match;
    }

    /**
     * Factory method that looks up Accid enum instance by its input string
     * representation.
     *
     * @param input User string input
     * @return Accid enum
     * @throws IllegalArgumentException if input invalid
     */
    public static Accid of(String input) throws IllegalArgumentException {
        Accid match = Accid.lookupByInput.get(input); 
        if (match == null) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized accidental input %s", input));
        }
        return match;
    }

    // Accessors
    private String input() {
        return this.input;
    }

    private String outputUnicode() {
        return this.outputUnicode;
    }

    private String outputLy() {
        return this.outputLy;
    }

    /**
     * Adjustment value in chromatic steps.
     *
     * @return Integer adjustment
     */
    public int adjustment() {
        return this.adjustment;
    }

    /**
     * Adjustment value in chromatic steps.
     *
     * @return Integer adjustment
     */
    public int offset12() {
        return this.adjustment();
    }

    /**
     * Unicode standard representation.
     *
     * @return String representation
     */
    public String toString() {
        return this.outputUnicode();
    }

    /**
     * Lilypond representation
     *
     * @return String Lilypond code
     */
    public String toLy() {
        return this.outputLy();
    }
}
