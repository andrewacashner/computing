package com.andrewcashner.musarithmetic;

import java.util.regex.*;

/*
 * Interval Adjustments
        perfect
            diminished -1
            perfect 0
            augmented 1
        imperfect
            diminished -2
            minor -1
            major 0
            augmented 1
 */   

record Interval(Quality quality, int degree) {
    // degree is zero-indexed and can be negative

    public Interval {
        if (!Interval.isValid(quality, degree)) {
            throw new IllegalArgumentException("Illegal interval degree-quality combination");
        }
    }

    private static enum QualityCategory { PERFECT, IMPERFECT };
    
    private static QualityCategory category(int degree) {
        return switch (Math.abs(degree) % 7) {
            case 0, 3, 4 -> QualityCategory.PERFECT;
            default      -> QualityCategory.IMPERFECT;
        };
    }

    private static boolean isValidImperfect(Quality quality) {
        return switch (quality) {
            case DIMINISHED, MINOR, MAJOR, AUGMENTED -> true;
            default -> false;
        };
    }

    private static boolean isValidPerfect(Quality quality) {
        return switch (quality) {
            case DIMINISHED, PERFECT, AUGMENTED -> true;
            default -> false;
        };
    }

    private static boolean isValid(Quality quality, int degree) {
        QualityCategory category = Interval.category(degree);
        return switch (category) {
            case IMPERFECT -> isValidImperfect(quality);
            case PERFECT   -> isValidPerfect(quality);
        };
    }

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
        QualityCategory category = Interval.category(this.degree());

        int adjustment = switch (this.quality()) {
            case DIMINISHED -> 
                (category == QualityCategory.IMPERFECT) ? -2 : -1;
            case MINOR          -> -1;
            case PERFECT, MAJOR ->  0;
            case AUGMENTED      ->  1;
        };

        int offset = Pitch.offset12(this.degree()) + adjustment;
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
