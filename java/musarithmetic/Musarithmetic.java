public class Musarithmetic {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java Musarithmetic PITCH1 PITCH2");
            return;
        }

        String p1Input = args[0];
        String p2Input = args[1];

        try {
            Pitch p1 = new Pitch(p1Input);
            System.out.println(p1);
            System.out.println(p1.toLy());

            Pitch p2 = new Pitch(p2Input);
            System.out.println(p1);
            System.out.println(p2.toLy());

// TODO Need concept of intervals (doesn't make sense to subtract pitches)
//            Pitch p3 = p1.diff(p2); 
//            System.out.format("%d - %d = %s\n", p1, p2, p3);
        } 
        catch (IllegalArgumentException e) {
            System.err.format("Could not create pitch from input \"%s %s\"\n"
                    + "  %s\n", p1Input, p2Input, e.getMessage());

        }
    }
}
