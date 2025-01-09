package com.andrewcashner.musarithmetic;

public class IntervalCalculator {
    public static String evaluate(String input) 
            throws IllegalArgumentException {

        String result = "";
       
        String[] words = input.split("\\s+");
        if (words.length != 3) {
            throw new IllegalArgumentException(
                    String.format("Invalid input %s", input));
        }

        try {
            Pitch p1 = Pitch.of(words[0]);
            Sign sign = Sign.of(words[1]);
            Interval interval = Interval.of(words[2], sign);
            Pitch p2 = Pitch.inc(p1, interval);
            result = p2.toString();
        }
        catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(String.format(
                        "Could not evaluate expression \"%s\"", input), e);
        }
        return result;
    }
}
