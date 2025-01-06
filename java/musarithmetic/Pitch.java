package com.andrewcashner.musarithmetic;

import java.util.regex.*;
import java.util.function.*;
import java.util.stream.*;

record Pitch(Pname pname, Accid accid, Octave octave) {
public Pitch() {
        this(Pname.DEFAULT, Accid.DEFAULT, Octave.DEFAULT);
    }

    private static Pitch of(Pname pname, Accid accid, Octave octave) {
        return new Pitch(pname, accid, octave);
    }

    private static Pitch of(String pnameStr, String accidStr, 
                String octaveStr) 
            throws IllegalArgumentException {

        Pname pname = Pname.of(pnameStr);
        Accid accid = Accid.of(accidStr);
        Octave octave = Octave.of(octaveStr);
        return new Pitch(pname, accid, octave);
    }

    public static Pitch of(String inputStr) 
            throws IllegalArgumentException {

        Pitch pitch;
        Pattern syntax = Pattern.compile("([a-gA-G])([#b]{0,2})([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);
       
        if (tokens.matches()) {
            pitch = Pitch.of(tokens.group(1), 
                    tokens.group(2), 
                    tokens.group(3));
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input \"%s\"", inputStr));
        }
        return pitch;
    }

    private String formatFields(Pitch p, 
                                Function<PitchComponent, String> fn) {

        return Stream.of(p.pname(), p.accid(), p.octave())
                .map(fn).collect(Collectors.joining());
    }

    public String toString() {
        return formatFields(this, PitchComponent::toString);
    }

    public String toLy() {
        return formatFields(this, PitchComponent::toLy);
    }

    public static int offset12(int offset7) {
        final int[] offsets = { 0, 2, 4, 5, 7, 9, 11 };
        int base = Math.abs(offset7) % 7;
        return offsets[base];
    }

    // smallest distance from zero
    // in base 12, 0 - 11 = 1, 11 - 0 = -1
    private int cycleDiff(int n, int m, int base) {
        int diff = n % base - m % base;
   
        Function<Integer, Integer> flip = 
            (a) -> a + base * Math.negateExact(Integer.signum(a));

        return (Math.abs(diff) > base / 2) ? flip.apply(diff) : diff;
    }

    private int value7() {
        return this.pname().offset();
    }

    private int value12() {
        int offset7 = this.value7();
        int offset12 = Pitch.offset12(offset7);
        int adjustment = this.accid().adjustment();
        int adjustedOffset12 = offset12 + adjustment;
        if (adjustedOffset12 < 0) {
            adjustedOffset12 += 12;
        }
        int wrappedOffset12 = adjustedOffset12 % 12;

        return wrappedOffset12;
    }

    private int octaveValue7() {
        return this.octave().offset7() + this.value7();
    }

    private int octaveValue12() {
        return this.octave().offset12() + this.value12();
    }

    // - add diatonic value of pitch and interval to get base note name
    // - add chromatic value of pitch and interval to get enharmonic
    //      chromatic pitch
    // - subtract chromatic value of new pitch from diatonic value to get
    //      accidental adjustment
    public static Pitch inc(Pitch pitch, Interval interval) {
        int target7 = pitch.octaveValue7() + interval.degree();
        int target12 = pitch.octaveValue12() + interval.offset12();

        Pname pname = Pname.of(target7 % 7);
        Octave octave = Octave.of(target7 / 7);
        
        int adjustment = cycleDiff(target12, pname.offset12(), 12);
        Accid accid = Accid.of(adjustment);

        return Pitch.of(pname, accid, octave);
    }
}
