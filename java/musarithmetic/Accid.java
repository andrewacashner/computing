import java.util.stream.*;
import java.util.List;

enum Accid implements PitchComponent {
    DBL_FLAT    ("bb",  "𝄪",   "eses",  -2),
    FLAT        ("b",   "♭",   "es",    -1),
    NATURAL     ("",    "",    "",       0),
    SHARP       ("#",   "♯",   "is",     1),
    DBL_SHARP   ("##",  "𝄫",   "isis",   2);

    private String input;
    private String outputUnicode;
    private String outputLy;
    private int adjustment;

    public static final Accid DEFAULT = Accid.NATURAL;

    private Accid(String input, String outputUnicode, String outputLy,
            int adjustment) {

        this.input = input;
        this.outputUnicode = outputUnicode;
        this.outputLy = outputLy;
        this.adjustment = adjustment;
    }

    public static Accid of(int adjustment) throws IllegalArgumentException {
        List<Accid> matches = 
            List.of(Accid.values()).stream()
            .filter(acc -> acc.adjustment == adjustment)
            .collect(Collectors.toList());
       
        Accid match;
        if (matches.size() == 1) {
            match = matches.get(0);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Invalid accidental value %d", adjustment));
        }
        return match;

    }

    public static Accid of(String input) throws IllegalArgumentException {
        List<Accid> matches = List.of(Accid.values()).stream()
                                   .filter(acc -> acc.input.equals(input))
                                   .collect(Collectors.toList());
        Accid match;
        if (matches.size() == 1) {
            match = matches.get(0);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized accidental input %s", input));
        }
        return match;
    }

    public String toString() {
        return this.outputUnicode;
    }

    public String toLy() {
        return this.outputLy;
    }

    public int getAdjustment() {
        return this.adjustment;
    }

}
