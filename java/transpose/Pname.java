public class Pname {
    char name;
    int offset;

    private char[] names = { 'C', 'D', 'E', 'F', 'G', 'A', 'B' };

    Pname(int offset) {
        if (offset >= 0 && offset <= 6) {
            this.offset = offset;
            this.name = names[offset];
        } else {
            throw new Error(
                    String.format("Pname offset %d out of bounds", offset));
        }
    }

    Pname(char name) {
        boolean found = false;
        int i;
        for (i = 0; i < this.names.length; ++i) {
            if (this.names[i] == name) {
                found = true;
                break;
            }
        }
        if (found) {
            this.name = name;
            this.offset = i;
        } else {
            throw new Error(
                    String.format("Illegal pitch name \'%c\'", name));
        }
    }

    public String toString() {
        return String.format("%c", this.name);
    }
}


