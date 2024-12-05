import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Collectors;

public class NumGenFunctional {

    public static void main(String[] args) {
        int max = parseArg(args);
        Random generator = new Random();

        String randomListString = 
            generator.ints(max, 0, max)
            .mapToObj(Integer::toString)
            .collect(Collectors.joining(" "));
        
        System.out.println(randomListString);

    }

    private static int parseArg(String[] args) {
        final int MAX_DEFAULT = 100;
        int max = MAX_DEFAULT;
        
        if (args.length > 0) {
            try {
                int test = Integer.parseInt(args[0]);
                max = test;
            }
            catch (NumberFormatException e) {
                System.err.format("Bad command-line argument (%s);" +
                        " using default value %d\n",
                        e.getMessage(), MAX_DEFAULT);
            }
        }
        return max;
    }
    
}
