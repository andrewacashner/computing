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
public enum Quality { 
    /** Diminished interval */
    DIMINISHED ("d"),
    /** Perfect */
    PERFECT    ("P"),
    /** Augmented */
    AUGMENTED  ("A"),
    /** Minor */
    MINOR      ("m"),
    /** Major */
    MAJOR      ("M");

    private String symbol;

    private Quality(String symbol) {
        this.symbol = symbol;
    }

    private String symbol() {
        return this.symbol;
    }

    /** Default Quality is perfect */
    public static final Quality DEFAULT = Quality.PERFECT;

    // Enable lookup by symbol
    private static final Map<String, Quality> lookup = new HashMap<>();

    static {
        Arrays.stream(Quality.values())
            .forEach(q -> lookup.put(q.symbol(), q));
    }

    /**
     * Factory method to look up Quality from an input string.
     *
     * @param input User input string
     * @return New Quality instance
     * @throws IllegalArgumentException if invalid input
     */
    public static Quality of(String input) throws IllegalArgumentException {
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
        return this.symbol();
    }
} 


