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

    private final static int[] offsets12 = {
        0, 2, 4, 5, 7, 9, 11
    };
   
    public static int offset12(int offset7) {
        int base = Math.abs(offset7) % 7;
        return Pitch.offsets12[base];
    }

    public int value7() {
        return this.pname().offset();
    }

    public int value12() {
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

    public int octaveValue7() {
        return this.octave().offset7() + this.value7();
    }

    public int octaveValue12() {
        return this.octave().offset12() + this.value12();
    }

    public static int diff7(Pitch first, Pitch second) {
        return first.value7() - second.value7();
    }

    public static int diff12(Pitch first, Pitch second) {
        return first.octaveValue12() - second.octaveValue12();
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
        
        int adjustment = (target12 % 12) - pname.offset12();
        Accid accid = Accid.of(adjustment);
       
        Octave octave = Octave.of(target7 / 7);

        return Pitch.of(pname, accid, octave);
    }
}
