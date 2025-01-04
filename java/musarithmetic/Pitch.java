package com.andrewcashner.musarithmetic;

import java.util.regex.*;

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
                        "Could not parse input %s", inputStr));
        }
        return pitch;
    }

    public String toString() {
        return String.format("%s%s%s",
                this.pname(),
                this.accid(),
                this.octave());
    }

    public String toLy() {
        return String.format("%s%s%s",
                this.pname().toLy() +
                this.accid().toLy() +
                this.octave().toLy());
    }

    private final static int[] chromaticOffsets = {
        0, 2, 4, 5, 7, 9, 11
    };
   
    public static int chromaticOffset(int diatonicOffset) {
        int base = Math.abs(diatonicOffset) % 7;
        return Pitch.chromaticOffsets[base];
    }

    // TODO 'get' prefix or not?
    public int diatonicValue() {
        return this.pname().getOffset();
    }

    public int chromaticValue() {
        int diatonicOffset = this.diatonicValue();
        int chromaticOffset = Pitch.chromaticOffset(diatonicOffset);
        int adjustment = this.accid().getAdjustment();
        int adjustedChromaticOffset = chromaticOffset + adjustment;
        if (adjustedChromaticOffset < 0) {
            adjustedChromaticOffset += 12;
        }
        int wrappedChromaticOffset = adjustedChromaticOffset % 12;

        return wrappedChromaticOffset;
    }

    public int diatonicOctaveValue() {
        return this.octave().offset7() + this.diatonicValue();
    }

    public int chromaticOctaveValue() {
        return this.octave().offset12() + this.chromaticValue();
    }

    public int diffDiatonic(Pitch other) {
        return this.diatonicValue() - other.diatonicValue();
    }

    public int diffChromatic(Pitch other) {
        return this.chromaticOctaveValue() - other.chromaticOctaveValue();
    }

    // - add diatonic value of pitch and interval to get base note name
    // - add chromatic value of pitch and interval to get enharmonic
    //      chromatic pitch
    // - subtract chromatic value of new pitch from diatonic value to get
    //      accidental adjustment
    public Pitch inc(Interval interval) {
        int diatonicTarget = this.diatonicOctaveValue() 
                                + interval.degree();

        int chromaticTarget = this.chromaticOctaveValue() 
                                + interval.chromaticOffset();

        Pname pname = Pname.of(diatonicTarget % 7);
        
        int adjustment = (chromaticTarget % 12) - pname.chromaticOffset();
        Accid accid = Accid.of(adjustment);
       
        Octave octave = Octave.of(diatonicTarget / 7);

        return Pitch.of(pname, accid, octave);
    }
}
