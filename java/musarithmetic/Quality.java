package com.andrewcashner.musarithmetic;

import java.util.*;

/**
 * A Quality is an inflection of an interval relative to a default base
 * value.
 * For perfect intervals the base value is PERFECT; for imperfect intervals
 * the base value is MAJOR.
 * Diminished offset values are different for imperfect and perfect
 * intervals; that code is in {@link Interval}.
 *
 * Note: we do not currently support double (or more) diminished intervals.
 */
public enum Quality implements Describable { 
    /** Diminished interval */
    DIMINISHED ("d", "diminished"),
    /** Perfect */
    PERFECT    ("P", "perfect"),
    /** Augmented */
    AUGMENTED  ("A", "augmented"),
    /** Minor */
    MINOR      ("m", "minor"),
    /** Major */
    MAJOR      ("M", "major");

    private String symbol;
    private String description;

    private Quality(String symbol, String description) {
        this.symbol = symbol;
        this.description = description;
    }

    /** Default Quality is perfect */
    public static final Quality DEFAULT = Quality.PERFECT;

    // Enable lookup by symbol
    private static final Map<String, Quality> lookup = new HashMap<>();

    static {
        Arrays.stream(Quality.values())
            .forEach(q -> lookup.put(q.symbol, q));
    }

    /**
     * Factory method to look up Quality from an input string.
     *
     * @param input User input string
     * @return New Quality instance
     * @throws IllegalArgumentException if invalid input
     */
    public static Quality parse(String input) 
            throws IllegalArgumentException {

        Quality match = lookup.get(input);
        if (input == null) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized quality input %s", input));
        }
        return match;
    }

    /**
     * String representation of the quality.
     *
     * @return String representation
     */
    public String toString() {
        return this.symbol;
    }

    public String description() {
        return this.description;
    }
} 
