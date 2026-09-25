import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Transposer
 *
 * Given a melody in ASCII format and a number of semitones, transpose the
 * melody. 
 *
 * ASCII Format: space delimited, each pitch is an upper case letter +
 * accidental # (sharp), b (flat), or no accidental for natural.  
 * No accidentals carry over to the next note.
 * Double sharps or double flats are possible, but not triple.
 *
 * Because we aren't specifing full intervals with qualities, we just render
 * the output as a standard enharmonic pitch.
 * So F plus 1 is F#, and so is G - 1.
 *
 * @author Andrew Cashner
 * @date 2026-09-25
 */
public class Transposer {
    private String original;
    private ArrayList<String> pitches;
    private int semitones;

    public Transposer(String melody, int semitones) throws Exception {
        this.pitches = new ArrayList<>(Arrays.asList(melody.split("\s")));
        checkValidity();

        this.original = melody;

        this.semitones = semitones % 12;
        if (this.semitones < 0) {
            this.semitones += 12;
        }
    }

    private void checkValidity() throws Exception {
        for (String pitch: this.pitches) {
            int error = 0;
            boolean isValid = "ABCDEFG".indexOf(pitch.charAt(0)) != -1;

            if (!isValid) {
                error = 1;
            }

            if (pitch.length() > 1) {

                String accidentals = pitch.substring(1);

                if (pitch.length() == 2) {
                    isValid = accidentals.equals("#") || accidentals.equals("b");

                    if (!isValid) {
                        error = 2;
                    }
                } else if (pitch.length() == 3) {
                    isValid = accidentals.equals("##") || accidentals.equals("bb");
                    if (!isValid) {
                        error = 3;
                    }
                } else {
                    isValid = false;
                    error = 4;
                }
            }

            if (!isValid) {
                throw new Exception(
                        String.format("Invalid pitch input '%s' (error code %d)", pitch, error));
            }
        }
    }

    private String transpose() {
        String transposed = this.pitches.stream().map(
                (String pitch) -> shift(pitch, this.semitones))
            .collect(Collectors.joining(" "));
        return transposed;
    }

    private String shift(String pitchStr, int semitones) {
        EnharmonicPitch pitch = new EnharmonicPitch(pitchStr);
        EnharmonicPitch transposed = pitch.inc(semitones);
        return transposed.toString();
    }

    private class EnharmonicPitch {
        private int keyNumber;

        public EnharmonicPitch(int keyNumber) {
            this.keyNumber = keyNumber % 12;
        }

        public EnharmonicPitch(String pitchStr) {
            final int[] diatonicBases = { 0, 2, 4, 5, 7, 9, 11 };

            int baseIndex = pitchStr.charAt(0) - 'A' - 2;
            if (baseIndex < 0) {
                baseIndex += 7;
            }

            this.keyNumber = diatonicBases[baseIndex];

            if (pitchStr.length() > 1) {
                String accidentals = pitchStr.substring(1);
                String[] accidentalStrings = { "#", "b", "##", "bb" };
               
                int adjustment = switch (accidentals) {
                    case "#"  ->  1;
                    case "b"  -> -1;
                    case "##" ->  2;
                    case "bb" -> -2;
                    default   ->  0;
                };

                this.keyNumber += adjustment;
            }
        }

        public String toString() {
            final String[] pitchStrings = { 
                "C", "C#", 
                "D", "D#", 
                "E", 
                "F", "F#", 
                "G", "G#", 
                "A", "A#", 
                "B" 
            };
            return pitchStrings[this.keyNumber];
        }

        public EnharmonicPitch inc(int semitone) {
            return new EnharmonicPitch(this.keyNumber + semitone);
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: transpose \"PITCH1 PITCH2 PITCH3\" [SEMITONES]");
        } else {
            try {
                String melody = args[0];
                int semitones = Integer.parseInt(args[1]);

                Transposer transposer = new Transposer(melody, semitones);
                String transposed = transposer.transpose();

                System.out.println(transposed);

            } catch (Exception ex) {
                System.err.println(ex);
            }
        }
    }
}
