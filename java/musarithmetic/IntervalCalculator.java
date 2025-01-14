package com.andrewcashner.musarithmetic;

/**
 * Evaluate an expression in which an interval is added to or subtraced from
 * a pitch, returning a new pitch.
 */
public class IntervalCalculator {

    // To appease javadoc
    private IntervalCalculator() {}

    /**
     * Three required command-line arguments are interpreted as a pitch, an
     * operation (+ or -), and an interval.
     * Calculate and display the resulting pitch.
     *
     * @param args Command-line arguments: pitch, operation, interval
     */
    public static void main(String[] args) {
        if (args.length != 3) {
            System.err.println("Usage: " +
                    "java Musarithmetic PITCH +/- INTERVAL");
            return;
        }

        String result = evaluate(args[0], args[1], args[2]);
        System.out.println(result);
    }
   
    /**
     * Evaluate an expression comprising a pitch, sign, and interval from
     * string inputs.
     *
     * @param pitchStr String containing pitch symbol
     * @param signStr String + or -
     * @param intervalStr String containing interval symbol
     * @return String representing result pitch
     * @throws IllegalArgumentException if any of the input strings do not
     *      parse
     */
    public static String evaluate(String pitchStr, String signStr, 
            String intervalStr) throws IllegalArgumentException {

        String result = "";
        try {
            Pitch p1 = Pitch.of(pitchStr);
            Sign sign = Sign.of(signStr);
            Interval interval = Interval.of(intervalStr, sign);
            Pitch p2 = Pitch.inc(p1, interval);
            result = p2.toString();
        }
        catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(String.format(
                        "Could not evaluate expression \"%s %s %s\"", 
                        pitchStr, signStr, intervalStr), e);
        }
        return result;
    }

    /**
     * Evaluate a whole expression from a single string.
     *
     * @param input String with full expression (pitch, sign, interval)
     * @return String representation of result pitch
     * @throws IllegalArgumentException if input is invalid
     */
    public static String evaluate(String input) 
            throws IllegalArgumentException {

        String[] words = input.split("\\s+");
        if (words.length != 3) {
            throw new IllegalArgumentException(
                    String.format("Invalid input %s", input));
        }

        return evaluate(words[0], words[1], words[2]);
    }
}
