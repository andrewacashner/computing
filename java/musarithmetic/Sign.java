package com.andrewcashner.musarithmetic;

enum Sign {

    POSITIVE (1),
    NEGATIVE (-1);

    private int value;

    private Sign(int value) {
        this.value = value;
    }

    private int value() {
        return this.value;
    }

    public static Sign of(String input) throws IllegalArgumentException {
        return switch (input) {
            case "+" -> Sign.POSITIVE;
            case "-" -> Sign.NEGATIVE;
            default -> throw new IllegalArgumentException(
                String.format("Could not create sign from input \"%s\"", 
                    input));
        };
    }

    private boolean isPositive() {
        return this == Sign.POSITIVE;
    }

    public int apply(int n) {
        return n * this.value();
    }

    public String toString() {
        return (this.isPositive()) ? "+" : "-";
    }
}
