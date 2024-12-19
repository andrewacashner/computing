import java.util.List;
import java.util.stream.*;

public enum Quality { 
    MINOR       (-1, "m"), 
    DIMINISHED  (-1, "d"), 
    MAJOR       ( 0, "M"), 
    PERFECT     ( 0, "P"), 
    AUGMENTED   ( 1, "A");

    private int adjustment;
    private String representation;

    private Quality(int adjustment, String representation) {
        this.adjustment = adjustment;
        this.representation = representation;
    }

    public static Quality of(String input) throws IllegalArgumentException {
        List<Quality> matches =
            List.of(Quality.values()).stream()
            .filter(q -> q.representation.equals(input))
            .collect(Collectors.toList());

        Quality match;
        if (matches.size() == 1) {
            match = matches.get(0);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized quality input %s", input));
        }
        return match;
    }

    public int getAdjustment() {
        return this.adjustment;
    }

    public String toString() {
        return this.representation;
    }
} 


