package com.andrewcashner.musarithmetic;

import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;

// TODO diminished and augmented are different for imperfect and perfect
// intervals!

enum Quality { 
    MINOR       (-1, "m"), 
    DIMINISHED  (-1, "d"), 
    MAJOR       ( 0, "M"), 
    PERFECT     ( 0, "P"), 
    AUGMENTED   ( 1, "A");

    private int adjustment;
    private String representation;

    private static final Map<String, Quality> lookup 
        = new HashMap<String, Quality>();

    static {
        Arrays.stream(Quality.values())
            .forEach(q -> lookup.put(q.getRepresentation(), q));
    }

    private Quality(int adjustment, String representation) {
        this.adjustment = adjustment;
        this.representation = representation;
    }

    public static Quality of(String input) throws IllegalArgumentException {
        Quality match = Quality.lookup.get(input);
        if (match == null) {
            throw new IllegalArgumentException(String.format(
                        "Unrecognized quality input %s", input));
        }
        return match;
    }

    public int getAdjustment() {
        return this.adjustment;
    }

    private String getRepresentation() {
        return this.representation;
    }

    public String toString() {
        return this.getRepresentation();
    }
} 


