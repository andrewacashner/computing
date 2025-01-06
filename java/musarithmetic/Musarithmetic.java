package com.andrewcashner.musarithmetic;

public class Musarithmetic {
    public static void main(String[] args) {
        if (args.length != 3) {
            System.err.println(
                    "Usage: java Musarithmetic PITCH +/- INTERVAL");
            return;
        }

        String pitchInput    = args[0];
        String signInput     = args[1];
        String intervalInput = args[2];

        try {
            Pitch p1 = Pitch.of(pitchInput);
            Sign sign = Sign.of(signInput);
            Interval interval = Interval.of(intervalInput, sign);

            Pitch p2 = Pitch.inc(p1, interval); 

            System.out.format("%s %s %s = %s [%s]\n", 
                    p1, sign, interval, p2, p2.toLy());
        } 
        catch (IllegalArgumentException e) {
            System.err.format(
                    "Could not calculate pitch from input \"%s\"\n  %s\n",
                    String.join(" ", args), e.getMessage());
        }
    }
}
