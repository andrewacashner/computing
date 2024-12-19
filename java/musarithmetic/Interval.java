import java.util.regex.*;

public class Interval {
    private Quality quality;
    private int degree; // can be negative

    public Interval(Quality quality, int degree) {
        this.quality = quality;
        this.degree = degree;
    }

    public Interval(String inputStr) throws IllegalArgumentException {
        Pattern syntax = Pattern.compile("([+-]??)([mMPA])([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);

        if (tokens.matches()) {
            this.quality = Quality.of(tokens.group(2));
            this.degree = Integer.parseInt(tokens.group(3));
            if (tokens.group(1).equals("-")) {
                this.degree *= -1;
            }
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input %s", inputStr));
        }
    }

    public int getDegree() {
        return this.degree;
    }

    public String getSign() {
        return (this.degree > 0) ? "+" : "-";
    }

    public int getChromaticOffset() {
        return Pitch.getChromaticOffset(Math.abs(this.degree))
                + this.quality.getAdjustment();
    }

    public String toString() {
        return String.format("%s%d", this.quality, this.degree);
    }

}
