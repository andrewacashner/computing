package com.andrewcashner.transpose;

import java.util.List;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Transposer: Given a melody in ASCII format and a number of semitones,
 * transpose the melody enharmonically. 
 *
 * ASCII Format: space delimited, each pitch is an upper case letter +
 * accidental # (sharp), b (flat), or no accidental for natural.  
 * No accidentals carry over to the next note.
 * Double sharps or double flats are possible, but not triple.
 *
 * Because we aren't specifying full intervals with qualities, we just render
 * the output as a standard enharmonic pitch.
 * So F plus 1 is F#, and so is G - 1.
 *
 * @author Andrew Cashner
 * @version 2026-09-25
 */
public class Transposer {
    private final List<EnharmonicPitch> pitches;
    private final int semitones;

    /**
     * Creates a Transposer from a string of pitches and a semitone amount o
     * transpose.
     *
     * @param melody A string of pitches
     * @param semitones Number of semitones to shift
     *
     * @throws IllegalArgumentException if input is invalid
     */
    public Transposer(String melody, int semitones) {
        if (melody == null || melody.isBlank()) {
            throw new IllegalArgumentException("Melody is empty");
        }

        this.pitches = Arrays.stream(melody.trim().split("\\s+"))
                             .map(EnharmonicPitch::parse)
                             .toList();
        this.semitones = Math.floorMod(semitones, 12);
    }

    private String transpose() {
        String transposed = this.pitches.stream()
            .map((EnharmonicPitch pitch) -> pitch.inc(this.semitones))
            .map(EnharmonicPitch::toString)
            .collect(Collectors.joining(" "));
        return transposed;
    }

    public static String transpose(String melody, int semitones) {
        Transposer transposer = new Transposer(melody, semitones);
        return transposer.transpose();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: transpose \"PITCH1 PITCH2 PITCH3\" SEMITONES");
            System.exit(1);
        } else {
            try {
                String melody = args[0];
                int semitones = Integer.parseInt(args[1]);

                String transposed = Transposer.transpose(melody, semitones);
                System.out.println(transposed);
            } catch (NumberFormatException ex) {
                System.err.println("Semitones must be an integer");
                System.exit(1);
            } catch (IllegalArgumentException ex) {
                System.err.println(ex.getMessage());
                System.exit(1);
            }
        }
    }

    private record EnharmonicPitch(int keyNumber) {
        private static final int[] DIATONIC_BASES = { 0, 2, 4, 5, 7, 9, 11 };
        private static final String[] PITCH_STRINGS = { 
            "C", "C#", 
            "D", "D#", 
            "E", 
            "F", "F#", 
            "G", "G#", 
            "A", "A#", 
            "B" 
        };

        EnharmonicPitch {
            keyNumber = Math.floorMod(keyNumber, 12);
        }

        /**
         * Create an EnharmonicPitch from a pitch string
         *
         * @param pitchStr Pitch input string
         * @throws IllegalArgumentException if input is invalid
         */
        public static EnharmonicPitch parse(String pitchStr) {
            int baseIndex = "CDEFGAB".indexOf(pitchStr.charAt(0));

            if (baseIndex == -1) {
                throw new IllegalArgumentException(
                        String.format("Invalid pitch name '%s': Pitch must be one of ABCDEFG", pitchStr));
            } 

            int keyNumber = DIATONIC_BASES[baseIndex];

            if (pitchStr.length() > 1) {
                String accidental = pitchStr.substring(1);
               
                int adjustment = switch (accidental) {
                    case "#"  ->  1;
                    case "b"  -> -1;
                    case "##" ->  2;
                    case "bb" -> -2;
                    default   ->  
                        throw new IllegalArgumentException(
                                String.format("Invalid accidental '%s': Accidental must be one of '#', 'b', '##', or 'bb'", accidental));
                };

                keyNumber += adjustment;
            }

            return new EnharmonicPitch(keyNumber);
        }

        @Override
        public String toString() {
            return PITCH_STRINGS[this.keyNumber];
        }

        public EnharmonicPitch inc(int semitone) {
            return new EnharmonicPitch(this.keyNumber + semitone);
        }
    }
}
