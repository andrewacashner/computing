public class Musarithmetic {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: java Musarithmetic PITCHNAME");
            return;
        }

        String pitchNameInput = args[0]; //toLowerCase();

        try {
            Pitch p1 = new Pitch(pitchNameInput);
            System.out.println(p1);

            Pitch p2 = new Pitch(Pname.of(1));
            System.out.println(p2);

            System.out.format("p2 - p1 = %d\n", 
                    p2.valueDiatonic() - p1.valueDiatonic());

            Pitch p3 = new Pitch();
            System.out.println(p3.toLy());
        } 
        catch (IllegalArgumentException e) {
            System.err.format("Could not create pitch from input \"%s\"\n"
                    + "  %s\n", pitchNameInput, e.getMessage());

        }
    }
}
