import java.util.List;
import java.util.ArrayList;

public class Accid {
    String name;
    int adjustment;

    private ArrayList<String> names = new ArrayList<>(
            List.of("𝄫", "♭", "♮", "♯", "𝄪"));

    private ArrayList<String> asciiNames = new ArrayList<>(
            List.of("bb", "b", "", "#", "##"));

    Accid(int adjustment) {
        if (adjustment >= -2 && adjustment <= 2) {
            this.adjustment = adjustment;
            this.name = names.get(adjustment + 2);
        } else {
            throw new Error(
                    String.format("Accid adjustment %d out of bounds", adjustment));
        }
    }

    Accid(String asciiName) {
        this.adjustment = asciiNames.indexOf(asciiName);
        if (this.adjustment != -1) {
            this.name = names.get(this.adjustment);
        } else {
            throw new Error(
                    String.format("Illegal accidental name %s", asciiName));
        } 
    }

    public String toString() {
        return this.name;
    }
}


