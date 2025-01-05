package com.andrewcashner.musarithmetic;

import java.util.regex.*;

record Interval(Quality quality, int degree) {
    // NB degree is zero-indexed and can be negative

    public static Interval of(String inputStr) 
            throws IllegalArgumentException {

        Interval interval;
        Pattern syntax = Pattern.compile("([+-]??)([mMPdA])([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);

        if (tokens.matches()) {
            Quality quality = Quality.of(tokens.group(2));

            // Adjust one-indexed interval notation to zero-indexed
            int degree = Integer.parseInt(tokens.group(3)) - 1;
            
            if (tokens.group(1).equals("-")) {
                degree *= -1;
                System.err.println("Negative interval");
            }
            interval = new Interval(quality, degree);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input %s", inputStr));
        }
        return interval;
    }

    public String sign() {
        return (this.degree() > 0) ? "+" : "-";
    }

    public int offset12() {
        int offset = Pitch.offset12(this.degree())
                        + this.quality().adjustment();
        if (this.degree() < 0) {
            offset *= -1;
        }
        return offset;
    }

    // Return to 1-indexed representation for display
    public String toString() {
        return String.format("%s%d", this.quality(), 
                Math.abs(this.degree()) + 1);
    }
}
