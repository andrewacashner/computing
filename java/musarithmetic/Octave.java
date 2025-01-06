package com.andrewcashner.musarithmetic;

record Octave(int octave) implements PitchComponent {
    public Octave {
        if (octave < 0 || octave > 10) {
            throw new IllegalArgumentException("Octave out of range");
        }
    }

    public final static Octave DEFAULT = new Octave(4);

    public static Octave of(int octave) {
        return new Octave(octave);
    }

    public static Octave of(String input) throws IllegalArgumentException {
        Octave octave;
        if (input.isEmpty()) {
            octave = Octave.DEFAULT;
        } else {
            try {
                int value = Integer.parseInt(input);
                octave = new Octave(value);
            }
            catch (NumberFormatException e) {
                throw new IllegalArgumentException(String.format(
                            "Could not create octave from input %s\n  %s",
                            input, e.getMessage()));
            }
        }
        return octave;
    }

    public int offset7() {
        return this.octave() * 7;
    }

    public int offset12() {
        return this.octave() * 12;
    }

    public String toString() {
        return Integer.toString(this.octave());
    }

    public String toLy() {
        String marker = this.octave < 3 ? "," : "'";
        return marker.repeat(Math.abs(this.octave - 3));
    }
}
