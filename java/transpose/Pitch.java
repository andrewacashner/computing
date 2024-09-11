public class Pitch {

    Pname pname;
    Accid accid;

    public Pitch(Pname pname, Accid accid) {
        this.pname = pname;
        this.accid = accid;
    }

    public Pitch(int offset, int adjustment) {
        this.pname = new Pname(offset);
        this.accid = new Accid(adjustment);
    }

    public Pitch(String input) {
        this.pname = new Pname(input.charAt(0));
        this.accid = new Accid(input.substring(1));
    }

    public Pitch incDiatonic(int interval) {
        int newPnameIndex = (this.pname.offset + interval) % 7;
        return new Pitch(newPnameIndex, this.accid.adjustment);
    }

    public String toString() {
        return String.format("%s%s", this.pname, this.accid);
    }
}



