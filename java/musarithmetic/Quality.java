package com.andrewcashner.musarithmetic;

import java.util.*;

// Diminished offset values are different for imperfect and perfect
// intervals; that code is in Interval.java

enum Quality { 
    DIMINISHED ("d"),
    PERFECT    ("P"),
    AUGMENTED  ("A"),
    MINOR      ("m"),
    MAJOR      ("M");

    String symbol;

    private Quality(String symbol) {
        this.symbol = symbol;
    }

    private String symbol() {
        return this.symbol;
    }

    private static final Map<String, Quality> lookup = new HashMap<>();

    static {
        Arrays.stream(Quality.values())
            .forEach(q -> lookup.put(q.symbol(), q));
    }

    public static Quality of(String input) throws IllegalArgumentException {
        Quality match = lookup.get(input);
        if (input == null) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized quality input %s", input));
        }
        return match;
    }

    public String toString() {
        return this.symbol;
    }
} 


