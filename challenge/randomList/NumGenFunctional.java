import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Collectors;

public class NumGenFunctional {

    public static void main(String[] args) {
        int max = parseArg(args);
        String output = streamToString(randomStream(max));
        System.out.println(output);
    }

//    String output = IntStream.range(0, max)
//                      .map(n -> generator.nextInt(max))
//                      .boxed().map(String::valueOf)
//                      .collect(Collectors.joining(" "));

    private static int parseArg(String[] args) {
        final int MAX_DEFAULT = 100;
        int max = MAX_DEFAULT;
        
        if (args.length > 0) {
            try {
                max = Integer.parseInt(args[0]);
            }
            catch (NumberFormatException e) {
                System.err.format("Bad command-line argument (%s);" +
                        " using default value %d\n",
                        e.getMessage(), MAX_DEFAULT);
                max = MAX_DEFAULT;
            }
        }
        return max;
    }
    
    private static IntStream randomStream(int max) {
        Random generator = new Random();
        return IntStream.range(0, max)
                .map(n -> generator.nextInt(max));
    }

    private static String streamToString(IntStream nums) {
        return nums.boxed().map(String::valueOf)
                .collect(Collectors.joining(" "));
    }
}
