package com.andrewcashner.musarithmetic;

import java.util.regex.*;

class Pitch {
    private Pname pname;
    private Accid accid;
    private int octave;

    public Pitch(Pname pname, Accid accid, int octave) {
        setPname(pname);
        setAccid(accid);
        setOctave(octave);
    }

    public Pitch() {
        this(Pname.DEFAULT, Accid.DEFAULT, Pitch.OCTAVE_DEFAULT);
    }

    private static Pitch of(String pnameStr, String accidStr, 
            String octaveStr) throws IllegalArgumentException {
        Pname pname = Pname.of(pnameStr);
        Accid accid = Accid.of(accidStr);
        int octave = Pitch.validOctave(octaveStr);
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

    private Pname getPname() {
        return this.pname;
    }

    private Accid getAccid() {
        return this.accid;
    }

    private int getOctave() {
        return this.octave;
    }

    public void setPname(Pname pname) {
        this.pname = pname;
    }

    public void setAccid(Accid accid) {
        this.accid = accid;
    }

    public void setOctave(int octave) {
        this.octave = octave;
    }

    private static int validOctave(String input) 
            throws IllegalArgumentException {

        int octave;
        if (input.isEmpty()) {
            octave = Pitch.OCTAVE_DEFAULT;
        } else {
            try {
                int value = Integer.parseInt(input);
                octave = value;
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(String.format(
                            "Could not create octave from input %s\n  %s",
                            input, e.getMessage()));
            }
        }

        return octave;
    }

    private static final int OCTAVE_DEFAULT = 4;

    private static String octaveToLy(int octave) {
        String marker = octave < 3 ? "," : "'";
        return marker.repeat(Math.abs(octave - 3));
    }

    private int octaveOffset7() {
        return this.getOctave() * 7;
    }

    private int octaveOffset12() {
        return this.getOctave() * 12;
    }

    public String toString() {
        return String.format("%s%s%d",
                this.pname,
                this.accid,
                this.octave);
    }

    public String toLy() {
        return String.format("%s%s%s",
                this.getPname().toLy() +
                this.getAccid().toLy() +
                Pitch.octaveToLy(this.getOctave()));
    }

    private final static int[] chromaticOffsets = {
        0, 2, 4, 5, 7, 9, 11
    };
   
    public static int chromaticOffset(int diatonicOffset) {
        int base = Math.abs(diatonicOffset) % 7;
        return Pitch.chromaticOffsets[base];
    }

    public int diatonicValue() {
        return this.pname.getOffset();
    }

    public int chromaticValue() {
        int diatonicOffset = this.diatonicValue();
        int chromaticOffset = Pitch.chromaticOffset(diatonicOffset);
        int adjustment = this.accid.getAdjustment();
        int adjustedChromaticOffset = chromaticOffset + adjustment;
        if (adjustedChromaticOffset < 0) {
            adjustedChromaticOffset += 12;
        }
        int wrappedChromaticOffset = adjustedChromaticOffset % 12;

        return wrappedChromaticOffset;
    }

    public int diatonicOctaveValue() {
        return this.getOctave() * 7 + this.diatonicValue();
    }

    public int chromaticOctaveValue() {
        return this.getOctave() * 12 + this.chromaticValue();
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
                                + interval.getDegree();

        int chromaticTarget = this.chromaticOctaveValue() 
                                + interval.chromaticOffset();

        Pname pname = Pname.of(diatonicTarget % 7);
        
        int adjustment = (chromaticTarget % 12) - pname.chromaticOffset();
        Accid accid = Accid.of(adjustment);
       
        int octave = diatonicTarget / 7;

        return new Pitch(pname, accid, octave);
    }
}
