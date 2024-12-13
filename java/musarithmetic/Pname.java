enum Pname implements PitchComponent {
    C, D, E, F, G, A, B;

    public static final Pname DEFAULT = Pname.C;

    public int getOffset() {
        return this.ordinal();
    }

    public static Pname of(int value) 
            throws IllegalArgumentException {

        int absValue = Math.abs(value);
        if (absValue < Pname.values().length) {
            return Pname.values()[absValue];
        } else {
            throw new IllegalArgumentException(String.format(
                        "Pname value %d out of bounds", value));
        }
    }

    public static Pname of(String pitchString) 
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

    public String toLy() {
        return this.toString().toLowerCase();
    }
}
