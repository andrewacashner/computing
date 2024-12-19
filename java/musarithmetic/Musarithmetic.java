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
            Pitch p1 = new Pitch(pitchInput);
            System.out.println(p1);
            System.out.println(p1.toLy());

            Interval interval = new Interval(intervalInput);
            Pitch p2 = p1.inc(interval); 
            System.out.format("%d %s %d = %s\n", 
                    p1, interval.getSign(), p2);
        } 
        catch (IllegalArgumentException e) {
            System.err.format(
                    "Could not calculate pitch from input \"%s %s\"\n"
                    + "  %s\n", pitchInput, intervalInput, e.getMessage());

        }
    }
}
