import java.util.regex.*;

public class Interval {
    private Quality quality;
    private int degree; // can be negative

    public Interval(Quality quality, int degree) {
        this.quality = quality;
        this.degree = degree;
    }

    public Interval(String inputStr) throws IllegalArgumentException {
        Pattern syntax = Pattern.compile("([+-]??)([mMPdA])([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);

        if (tokens.matches()) {
            this.quality = Quality.of(tokens.group(2));

            // Adjust one-indexed interval notation to zero-indexed
            this.degree = Integer.parseInt(tokens.group(3)) - 1;
            
            if (tokens.group(1).equals("-")) {
                this.degree *= -1;
                System.err.println("Negative interval");
            }
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input %s", inputStr));
        }
    }

    public int getDegree() {
        return this.degree;
    }

    public Quality getQuality() {
        return this.quality;
    }

    public String getSign() {
        return (this.degree > 0) ? "+" : "-";
    }

    public int getChromaticOffset() {
        int offset = Pitch.getChromaticOffset(this.getDegree())
                        + this.getQuality().getAdjustment();
        if (this.getDegree() < 0) {
            offset *= -1;
        }
        return offset;
    }

    // Return to 1-indexed representation for display
    public String toString() {
        return String.format("%s%d", this.quality, 
                Math.abs(this.degree) + 1);
    }

}
