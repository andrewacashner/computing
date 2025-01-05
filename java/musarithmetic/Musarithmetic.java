package com.andrewcashner.musarithmetic;

public class Musarithmetic {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println(
                    "Usage: java Musarithmetic PITCH +/-INTERVAL");
            return;
        }

        String pitchInput = args[0];
        String intervalInput = args[1];

        try {
            Pitch p1 = Pitch.of(pitchInput);
            Interval interval = Interval.of(intervalInput);

            Pitch p2 = Pitch.inc(p1, interval); 

            System.out.format("%s %s %s = %s\n", 
                    p1, interval.sign(), interval, p2);
        } 
        catch (IllegalArgumentException e) {
            System.err.format(
                    "Could not calculate pitch from input \"%s %s\"\n"
                    + "  %s\n",
                    pitchInput, intervalInput, e.getMessage());

        }
    }
}
