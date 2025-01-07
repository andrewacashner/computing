package com.andrewcashner.musarithmetic;

import java.util.function.*;
import java.util.regex.*;
import java.util.stream.*;

/**
 * We model a musical pitch using a record holding a pitch name, accidental,
 * and octave number. Factory methods allow pitches to be created from these
 * objects or from raw input strings that will be parsed.
 *
 * @param pname Pname object with pitch name
 * @param accid Accid object with accidental
 * @param octave Octave object with Helmholtz octave number
 */
public record Pitch(Pname pname, Accid accid, Octave octave) {

    /** 
     * Default pitch 
     */
    public Pitch() {
        this(Pname.DEFAULT, Accid.DEFAULT, Octave.DEFAULT);
    }

    /**
     * Factory method to create a Pitch from raw user input. Examples of
     * valid input would by "c", "c#4", and "cbb5".
     *
     * @param inputStr User input string
     * @return new Pitch instance
     * @throws IllegalArgumentException if user input cannot be parsed
     * correctly
     */
    public static Pitch of(String inputStr) 
            throws IllegalArgumentException {

        Pitch pitch;
        Pattern syntax = Pattern.compile("([a-gA-G])([#b]{0,2})([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);
       
        if (tokens.matches()) {
            Pname pname = Pname.of(tokens.group(1));
            Accid accid = Accid.of(tokens.group(2));
            Octave octave = Octave.of(tokens.group(3));
            pitch = new Pitch(pname, accid, octave);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input \"%s\"", inputStr));
        }
        return pitch;
    }

    /**
     * Each field of the record implements the PitchComponent interface and
     * therefore provides both toString() and toLy() methods.
     * So we can print in either standard or Lilypond output formats by
     * calling those methods on each field.
     *
     * @param p Pitch to print
     * @param fn Function taking a PitchComponent object and returning a
     *      string 
     * @return String representation of Pitch
     */
    private String formatFields(Pitch p, 
                    Function<PitchComponent, String> fn) {

        return Stream.of(p.pname(), p.accid(), p.octave())
                .map(fn).collect(Collectors.joining());
    }

    /**
     * Return a standard string representation of the Pitch by calling to
     * toString() method of each field.
     *
     * @return String representation
     */
    public String toString() {
        return formatFields(this, PitchComponent::toString);
    }

    /**
     * Return valid Lilypond code for this Pitch by calling to the toLy()
     * method of each field.
     *
     * @return Lilypond string representation
     */
    public String toLy() {
        return formatFields(this, PitchComponent::toLy);
    }

    /**
     * Given a zero-indexed diatonic offset from C, return the equivalent
     * chromatic offset for the same pitch. For example, D is offset 1 from C
     * in a diatonic scale, but is offset 2 from C in a chromatic scale.
     *
     * @param offset7 Diatonic offset (zero-indexed)
     * @return Chromatic offset (zero-indexed)
     */
    public static int offset12(int offset7) {
        final int[] offsets = { 0, 2, 4, 5, 7, 9, 11 };
        int base = Math.abs(offset7) % 7;
        return offsets[base];
    }

    /**
     * Compute the smallest distance between two points on a number circle of
     * a given base.
     * In base 12, <code>0 - 11 = 1</code>, <code>11 - 0 = -1</code>.
     */
    private static int cycleDiff(int n, int m, int base) {
        int diff = n % base - m % base;
   
        Function<Integer, Integer> flip = 
            (a) -> a + base * Math.negateExact(Integer.signum(a));

        return (Math.abs(diff) > base / 2) ? flip.apply(diff) : diff;
    }

    /**
     * Diatonic value of this pitch including octave. That is, a diatonic
     * (base 7) offset from C0, zero-indexed.
     *
     * @return int pitch number in diatonic scale
     */
    public int octaveValue7() {
        return this.octave().offset7() + this.pname().offset7();
    }

    /**
     * Chromatic value of this pitch including octave. That is, a chromatic 
     * (base 12) offset from C0, zero-indexed.
     *
     * @return int pitch number in chromatic scale
     */
    public int octaveValue12() {
        return Stream.of(this.octave(), this.pname(), this.accid())
                .mapToInt(PitchComponent::offset12)
                .sum();
    }

    /**
     * Add an Interval to a Pitch, returning a new Pitch.
     * <ol>
     * <li>To compute the correct resulting pitch name, we add the diatonic
     * value of the interval to the diatonic value of the starting pitch: 
     * <code>C + 5 = G</code>, <code>C - 3 = A</code>.</li>
     * <li>To compute the octave, we first calculate the resulting enharmonic
     * chromatic pitch by adding the chromatic value of the interval
     * to the chromatic value of the pitch (including the octave).
     * We divide the enharmonic pitch by 12 to get the octave:
     * <code>B4 + M2 = (4 * 12 + 11) + (2) = 61 -&gt; 61 % 12 = 5</code>
     * </li>
     * <li>
     * The accidental reflects the difference between the chromatic pitch and
     * diatonic sums. If we add a second to C, the result will always be some
     * kind of D (diatonic C + second = diatonic D because 0 + 1 = 1). If we
     * add a minor second to C, the chromatic result would be offset 1 from
     * C (that is, C#/Db, up one key on the keyboard), but the diatonic
     * result would be D. The chromatic offset of D is 2, and the difference
     * between the chromatic and diatonic results is -1, which corresponds
     * to a flat accidental, so the result is Db. 
     * Accidental calculations require arithmetic on a number circle, where
     * <code>0 - 11 = 1</code>.
     * </li>
     * </ol>
     *
     * @param pitch Starting Pitch
     * @param interval Interval to add (can be negative)
     * @return New Pitch
     */
    public static Pitch inc(Pitch pitch, Interval interval) {
        int target7 = pitch.octaveValue7() + interval.degree();
        int target12 = pitch.octaveValue12() + interval.offset12();

        Pname pname = Pname.of(target7 % 7);
        Octave octave = Octave.of(target7 / 7);
        
        int adjustment = Pitch.cycleDiff(target12, pname.offset12(), 12);
        Accid accid = Accid.of(adjustment);

        return new Pitch(pname, accid, octave);
    }
}
