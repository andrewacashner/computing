package com.andrewcashner.musarithmetic;

/**
 * A Pname is a diatonic pitch name A-G, zero-indexed starting from C (as in
 * a C-major scale).
 */
public enum Pname implements PitchComponent {
    /** Pitch C */
    C, 
    /** Pitch D */
    D, 
    /** Pitch E */
    E, 
    /** Pitch F */
    F, 
    /** Pitch G */
    G, 
    /** Pitch A */
    A, 
    /** Pitch B */
    B;

    /** The default pitch name is C. */
    public static final Pname DEFAULT = Pname.C;

    /** 
     * Factory method to look up a pitch name from its offset value. 
     *
     * @param value Diatonic offset value
     * @return Matching Pname enum
     * @throws IllegalArgumentException if value is out of range
     */
    public static Pname fromValue(int value) 
            throws IllegalArgumentException {

        int absValue = Math.abs(value);
        if (absValue < Pname.values().length) {
            return Pname.values()[absValue];
        } else {
            throw new IllegalArgumentException(String.format(
                        "Pname value %d out of bounds", value));
        }
    }

    /**
     * Factory method to look up a pitch name from its string input
     * representation.
     *
     * @param pitchString Input string
     * @return Matching Pname enum
     * @throws IllegalArgumentException if not found
     */
    public static Pname parse(String pitchString) 
            throws IllegalArgumentException {

        Pname pname;
        try {
            pname = Pname.valueOf(pitchString.toUpperCase());
        }
        catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized pitch input \"%s\"\n  %s", 
                        pitchString, e.getMessage()));
        }
        return pname;
    }

    /** 
     * Zero-indexed diatonic offset from C 
     *
     * @return Integer diatonic offset
     */
    public int offset7() {
        return this.ordinal();
    }

    /**
     * Chromatic offset corresponding to the base (natural) value at this
     * pitch name, zero-indexed from C.
     * See {@link Pitch#offset12(int)}.
     *
     * @return Chromatic pitch number
     */
    public int offset12() {
        return Pitch.convertOffset7to12(this.ordinal());
    }

    /**
     * Return Lilypond representation of this pitch name (just the lowercase
     * letter).
     *
     * @return String Lilypond code
     */
    public String toLy() {
        return this.toString().toLowerCase();
    }

    public String description() {
        return this.toString();
    }
}
