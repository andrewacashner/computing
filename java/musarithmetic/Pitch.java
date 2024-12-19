import java.util.stream.*;
import java.util.List;
import java.util.regex.*;

class Pitch {
    private Pname pname;
    private Accid accid;
    private Octave octave;

    private final static int[] chromaticOffsets = {
        0, 2, 4, 5, 7, 9, 11
    };

    public Pitch(Pname pname, Accid accid, Octave octave) {
        this.pname = pname;
        this.accid = accid;
        this.octave = octave;
    }

    public Pitch(String inputStr) throws IllegalArgumentException {
        Pattern syntax = Pattern.compile("([a-gA-G])([#b]{0,2})([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);
       
        if (tokens.matches()) {
            this.pname = Pname.of(tokens.group(1));
            this.accid = Accid.of(tokens.group(2));
            this.octave = Octave.of(tokens.group(3));
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input %s", inputStr));
        }
    }

    public Pitch() {
        this(Pname.DEFAULT, Accid.DEFAULT, Octave.DEFAULT);
    }
   
    public String toString() {
        return String.format("%s%s%s",
                this.pname,
                this.accid,
                this.octave);
    }

    public String toLy() {
        return List.of(
                this.pname, 
                this.accid, 
                this.octave
                ).stream().map(PitchComponent::toLy)
                 .collect(Collectors.joining());
    }

    public static int getChromaticOffset(int diatonicOffset) {
        return Pitch.chromaticOffsets[diatonicOffset];
    }

    public int diatonicValue() {
        return this.pname.getOffset();
    }

    public int chromaticValue() {
        int diatonicOffset = this.diatonicValue();
        int chromaticOffset = Pitch.getChromaticOffset(diatonicOffset);
        int adjustment = this.accid.getAdjustment();
        int adjustedChromaticOffset = chromaticOffset + adjustment;
        if (adjustedChromaticOffset < 0) {
            System.err.format("adjusting: offset %d, adjustment %d\n",
                    chromaticOffset, adjustment);
            adjustedChromaticOffset += 12;
        }
        int wrappedChromaticOffset = adjustedChromaticOffset % 12;

        System.err.format("diatonic offset %d, chromatic offset %d, adjusted chromatic offset %d, wrapped chromatic offset %d\n", diatonicOffset, chromaticOffset, adjustedChromaticOffset, wrappedChromaticOffset);

        return wrappedChromaticOffset;
    }

    public int diatonicOctaveValue() {
        return this.octave.diatonicOffset() + this.diatonicValue();
    }

    public int chromaticOctaveValue() {
        return this.octave.chromaticOffset() + this.chromaticValue();
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
                                + interval.getChromaticOffset();

        Pname pname = Pname.of(diatonicTarget % 7);

        int adjustment = chromaticTarget - diatonicTarget;
        Accid accid = Accid.of(adjustment);

        Octave octave = new Octave(diatonicTarget / 7);

        return new Pitch(pname, accid, octave);
    }
}
