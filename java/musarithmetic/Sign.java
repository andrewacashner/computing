package com.andrewcashner.musarithmetic;

/**
 * Represents the sign value (e.g., of an interval).
 */
public enum Sign {

    /** Positive sign */
    POSITIVE (1),
    /** Negative */
    NEGATIVE (-1);

    private int value;

    private Sign(int value) {
        this.value = value;
    }

    /** Default sign is positive */
    public static final Sign DEFAULT = Sign.POSITIVE;

    private int value() {
        return this.value;
    }

    /**
     * Factory method to create a Sign from string input ("+" or "-")
     *
     * @param input User input string
     * @return Sign instance
     * @throws IllegalArgumentException if invalid input 
     */
    public static Sign of(String input) throws IllegalArgumentException {
        return switch (input) {
            case "+" -> Sign.POSITIVE;
            case "-" -> Sign.NEGATIVE;
            default -> throw new IllegalArgumentException(
                String.format("Could not create sign from input \"%s\"", 
                    input));
        };
    }

    /**
     * Apply this sign to a given integer.
     *
     * @param n Integer value
     * @return Value with sign applied
     */
    public int apply(int n) {
        return n * this.value();
    }

    /**
     * String representation of sign, "+" or "-"
     *
     * @return String representation
     */
    public String toString() {
        return (this == Sign.POSITIVE) ? "+" : "-";
    }
}
