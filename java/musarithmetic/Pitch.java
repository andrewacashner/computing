class Pitch {
    private Pname pname;
//    private Accid accid;
//    private Octave octave;
//    private Dur dur;

    public Pitch(Pname pname) {
            // , Accid accid, Octave octave, Dur dur) {
        this.pname = pname;
//        this.accid = accid;
//        this.octave = octave;
//        this.dur = dur;
    }

    public Pitch(String pnameStr) throws IllegalArgumentException {
        this.pname = Pname.of(pnameStr);
    }

//    public Pitch(Pname pname, Octave octave) {
//        this(pname, Accid.DEFAULT, octave, Dur.DEFAULT);
//    }

    public Pitch() {
        this(Pname.DEFAULT);
        // , Accid.DEFAULT, Octave.DEFAULT, Dur.DEFAULT);
    }

   
    public String toString() {
        return this.pname.toString();
    }

    public String toLy() {
        return this.pname.toLy();
//            + accid.toStringLy() + 
//            octave.toStringLy() + dur.toStringLy(); 
        // TODO map
    }

    public int valueDiatonic() {
        return this.pname.offset();
    }
}
