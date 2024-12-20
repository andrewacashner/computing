class Octave implements PitchComponent {
    private int octave;

    public static final Octave DEFAULT = new Octave(4);

    public Octave(int octave) {
        this.octave = octave;
    }

    public int value() {
        return this.octave;
    }

    public int getDiatonicOffset() {
        return this.octave * 7;
    }

    public int getChromaticOffset() {
        return this.octave * 12;
    }

    public static Octave of(String input) throws IllegalArgumentException {
        int value;
        Octave newOct;
        if (input.isEmpty()) {
            newOct = Octave.DEFAULT;
        } else {
            try {
                value = Integer.parseInt(input);
                newOct = new Octave(value);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(String.format(
                            "Could not create octave from input %s\n  %s",
                            input, e.getMessage()));
            }
        }

        return newOct;
    }

    public String toString() {
        return Integer.toString(this.octave);
    }

    public String toLy() {
        String marker = this.octave < 3 ? "," : "'";
        return marker.repeat(Math.abs(this.octave - 3));
    }
}
