import java.util.Random;

public class NumGen {
    public static void main(String[] args) {
        final int MAX_DEFAULT = 100;
        int max = MAX_DEFAULT;

        if (args.length > 0) {
            try {
                max = Integer.parseInt(args[0]);
            }
            catch (NumberFormatException e) {
                System.err.format("Bad command-line argument (%s); using default value %d\n",
                        e.getMessage(), MAX_DEFAULT);
                max = MAX_DEFAULT;
            }
        }

        Random randGen = new Random();

        for (int i = 0; i < max; ++i) {
            int thisRand = randGen.nextInt(max);
            System.out.format("%d", thisRand);
            if (i < max - 1) {
                System.out.print(" ");
            }
        }
        System.out.println();
    }
}
