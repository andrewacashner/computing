public class Accid {
    String name;
    int adjustment;

    private String[] names = { "𝄫", "♭", "♮", "♯", "𝄪" };
    private String[] asciiNames = { "bb", "b", "", "#", "##" };

    Accid(int adjustment) {
        if (adjustment >= -2 && adjustment <= 2) {
            this.adjustment = adjustment;
            this.name = this.names[adjustment + 2];
        } else {
            throw new Error(
                    String.format("Accid adjustment %d out of bounds", adjustment));
        }
    }

    Accid(String asciiName) {
        boolean found = false;
        int i;
        for (i = 0; i < this.asciiNames.length; ++i) {
            if (this.asciiNames[i].equals(asciiName)) {
                found = true;
                break;
            }
        }
        if (found) {
            this.adjustment = i - 2;
            this.name = this.names[i];
        } else {
            throw new Error(
                    String.format("Illegal accidental name %s", asciiName));
        }
    }

    public String toString() {
        return this.name;
    }
}


