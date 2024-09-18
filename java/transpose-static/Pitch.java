class Pitch {
    int pname;
    int accid;
    int octave;

    public Pitch(int pname, int accid, int octave) {
        this.pname = pname;
        this.accid = accid;
        this.octave = octave;
    }

    public Pitch(String pnameStr, String accidStr, int octave) {
        this.pname = Gamut.pname.valueOf(pnameStr);
        this.accid = Gamut.accid.valueOf(accidStr);
        this.octave = octave;
    }

    public static Pitch parse(String rawInput) {
        int last = rawInput.length() - 1;
        String pnameStr = rawInput.substring(0, 1);
        String accidStr = rawInput.substring(1, last);
        String octaveStr = rawInput.substring(last);
        int octave = Integer.parseInt(octaveStr);

        return new Pitch(pnameStr, accidStr, octave);
    }

    public String toString() {
        return String.format("%s%s%d",
                Gamut.pname.get(this.pname),
                Gamut.accid.get(this.accid),
                this.octave);
    }
}
