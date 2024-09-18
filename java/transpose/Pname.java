import java.util.List;
import java.util.ArrayList;

public class Pname {
    char name;
    int offset;

    private char[] names = { 'C', 'D', 'E', 'F', 'G', 'A', 'B' };

    Pname(int offset) {
        if (offset >= 0 && offset <= 6) {
            this.offset = offset;
            this.name = names.get(offset);
        } else {
            throw new Error(
                    String.format("Pname offset %d out of bounds", offset));
        }
    }

    Pname(char name) {
        this.offset = names.indexOf(name);
        if (this.offset != -1) {
            this.name = names.get(this.offset);
        } else {
            throw new Error(
                    String.format("Illegal pitch name \'%c\'", name));
        }
    }

    public String toString() {
        return String.format("%c", this.name);
    }
}


