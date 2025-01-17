package com.andrewcashner.musarithmetic;

/**
 * The Octave record holds the Helmholtz octave number (where middle C
 * (<code>c'</code> in Lilypond) is C4).
 *
 * @param octave Integer octave number
 */
public record Octave(int octave) implements PitchComponent {
    /**
     * Default constructor
     * @param octave Integer Helmholtz octave
     * @throws IllegalArgumentException if octave out of range
     */
    public Octave {
        if (octave < Octave.MIN_VALUE || octave > Octave.MAX_VALUE) {
            throw new IllegalArgumentException("Octave out of range");
        }
    }

    public Octave() {
        this(DEFAULT_VALUE);
    }

    /** Default octave is 4 (middle C). */
    public final static int DEFAULT_VALUE = 4;
    public final static int MIN_VALUE = 0;
    public final static int MAX_VALUE = 10;

    /**
     * Factory method creating Octave from user input.
     * 
     * @param input User input string for octave number
     * @return New Octave instance
     * @throws IllegalArgumentException if input could not be parsed or is
     *      out of range
     */
    public static Octave parse(String input) 
            throws IllegalArgumentException {

        Octave octave;
        if (input.isEmpty()) {
            octave = new Octave();
        } else {
            try {
                int value = Integer.parseInt(input);
                octave = new Octave(value);
            }
            catch (IllegalArgumentException e) {
                throw new IllegalArgumentException(String.format(
                            "Could not create octave from input %s\n  %s",
                            input, e.getMessage()));
            }
        }
        return octave;
    }

    /**
     * The diatonic offset starting value for this octave (equivalent to the
     * diatonic pitch offset of C in this octave).
     *
     * @return Integer diatonic offset from C0
     */
    public int offset7() {
        return this.octave() * 7;
    }

    /**
     * The chromatic offset starting value for this octave (equivalent to
     * the chromatic pitch offset of C in this octave).
     *
     * @return Integer chromatic offset from C0
     */
    public int offset12() {
        return this.octave() * 12;
    }

    /**
     * Simple string representation is just the numeral.
     *
     * @return Standard string representation
     */
    public String toString() {
        return Integer.toString(this.octave());
    }

    /**
     * Lilypond code uses tick marks above and below for octaves, relative
     * to C3. (C4 is <code>c'</code>, C2 is <code>c,</code>).
     *
     * @return Lilypond code string
     */
    public String toLy() {
        String marker = this.octave < 3 ? "," : "'";
        return marker.repeat(Math.abs(this.octave - 3));
    }

    public String description() {
        return this.toString();
    }
}
