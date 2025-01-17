package com.andrewcashner.musarithmetic;

import java.util.regex.*;
import java.util.function.*;

/**
 * An Interval contains a Quality (diminished, minor, major, perfect, or
 * augmented) and a degree (a number indicating a diatonic offset from the
 * starting pitch).
 * The qualities are either perfect (unison, fourth, and fifth) or 
 * imperfect.
 * The allowable intervals and resulting adjustments are different for each
 * category:
 * <table>
 * <caption>Allowed Qualities and their Adjustments</caption>
 * <thead>
 *   <tr><th>Quality Category</th>
 *       <th>Allowed Quality</th>
 *       <th>Adjustment</th></tr>
 * </thead>
 * <tbody>
 *   <tr><td>Perfect</td>   <td></td>           <td></td>   </tr>
 *   <tr><td></td>          <td>Diminished</td> <td>-1</td> </tr>
 *   <tr><td></td>          <td>Perfect</td>    <td>0</td>  </tr>
 *   <tr><td></td>          <td>Augmented</td>  <td>1</td>  </tr>
 *   <tr><td>Imperfect</td> <td></td>           <td></td>   </tr>
 *   <tr><td></td>          <td>Diminished</td> <td>-2</td> </tr>
 *   <tr><td></td>          <td>Minor</td>      <td>-1</td> </tr>
 *   <tr><td></td>          <td>Major</td>      <td>0</td>  </tr>
 *   <tr><td></td>          <td>Augmented</td>  <td>1</td>  </tr>
 * </tbody>
 * </table>
 *
 * @param quality Quality instance
 * @param degree Integer diatonic offset from starting pitch:
 *      Zero-indexed, can be negative
 */   
public record Interval(Quality quality, int degree) {

    /**
     * Default constructor
     * @param quality Quality instance
     * @param degree Integer diatonic offset
     * @throws IllegalArgumentException if an illegal quality-degree
     *      combination is requested
     */
    public Interval {
        if (!Interval.isValid(quality, degree)) {
            throw new IllegalArgumentException("Illegal interval degree-quality combination");
        }
    }

    /** Default degree */
    public static final int DEFAULT_DEGREE = 0;
    
    /** Default interval is perfect unison */
    public Interval() {
        this(Quality.PERFECT, DEFAULT_DEGREE);
    }

    public Interval copyWith(Quality quality) {
        return new Interval(quality, this.degree());
    }

    public Interval copyWith(int degree) {
        return new Interval(this.quality(), degree);
    }

    public Interval signed(Sign sign) {
        return new Interval(this.quality(), sign.apply(this.degree()));
    }

    private static enum QualityCategory { PERFECT, IMPERFECT };
   
    // Unison, fourth, and fifth (zero-indexed 0, 3, 4) are perfect
    private static QualityCategory category(int degree) {
        return switch (Math.abs(degree) % 7) {
            case 0, 3, 4 -> QualityCategory.PERFECT;
            default      -> QualityCategory.IMPERFECT;
        };
    }

    // Check for valid combination of quality and degree
    public static boolean isValid(Quality quality, int degree) {

        Predicate<Quality> isValidImperfect = (qual) ->
            switch (qual) {
                case DIMINISHED, MINOR, MAJOR, AUGMENTED -> true;
                default -> false;
            };

        Predicate<Quality> isValidPerfect = (qual) ->
            switch (qual) {
                case DIMINISHED, PERFECT, AUGMENTED -> true;
                default -> false;
            };

        QualityCategory category = Interval.category(degree);
        return switch (category) {
            case IMPERFECT -> isValidImperfect.test(quality);
            case PERFECT   -> isValidPerfect.test(quality);
        };
    }

    /**
     * Factory method to create Interval given user input string and parsed
     * sign.
     * Examples of input: "m6", "P5", "d7"
     *
     * @param inputStr User string input for interval
     * @param sign Sign object parsed from user input
     * @return new Interval instance
     * @throws IllegalArgumentException if invalid input
     */
    public static Interval parse(String inputStr, Sign sign)
            throws IllegalArgumentException {

        Interval interval;
        Pattern syntax = Pattern.compile("([mMPdA])([0-9]*)");
        Matcher tokens = syntax.matcher(inputStr);

        if (tokens.matches()) {
            Quality quality = Quality.parse(tokens.group(1));

            int degree = Integer.parseInt(tokens.group(2));

            // Adjust one-indexed interval notation to zero-indexed and
            // make negative if need be
            degree = sign.apply(degree - 1);

            interval = new Interval(quality, degree);
        } else {
            throw new IllegalArgumentException(String.format(
                        "Could not parse input \"%s\"", inputStr));
        }
        return interval;
    }

    /** 
     * Create an Interval from an input string, assuming positive sign by
     * default.
     *
     * @param inputStr User input
     * @return New Interval instance
     */
    public static Interval parse(String inputStr) {
        return Interval.parse(inputStr, Sign.POSITIVE);
    }

    /**
     * String representation of the sign of this Interval.
     *
     * @return String "+" or "-"
     */
    public String sign() {
        return (this.degree() > 0) ? "+" : "-";
    }

    /**
     * Chromatic offset equivalent to this interval, computed from its base
     * degree plus the adjustment indicated by the quality.
     *
     * @return Integer zero-indexed chromatic offset (i.e., interval size)
     */
    public int offset12() {
        QualityCategory category = Interval.category(this.degree());

        int adjustment = switch (this.quality()) {
            case DIMINISHED -> 
                (category == QualityCategory.IMPERFECT) ? -2 : -1;
            case MINOR          -> -1;
            case PERFECT, MAJOR ->  0;
            case AUGMENTED      ->  1;
        };

        int offset = Pitch.convertOffset7to12(this.degree()) + adjustment;
        if (this.degree() < 0) {
            offset *= -1;
        }

        return offset;
    }

    /**
     * Go back to 1-indexed representation for display
     *
     * @return String representation
     */
    public String toString() {
        String output;
        if (this.degree() < 0) {
            output = String.format("%s%d", this.quality(),
                    this.degree() - 1);
        } else {
            output = String.format("%s%d", this.quality(), 
                Math.abs(this.degree()) + 1);
        }
        return output;
    }
}
