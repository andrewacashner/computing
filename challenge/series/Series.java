import java.util.stream.IntStream;
import java.util.function.IntToDoubleFunction;

public class Series {
    public static void main (String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: Series MIN MAX");
            return;
        }
        int min = Integer.parseInt(args[0]);
        int max = Integer.parseInt(args[1]);
        
//        double sum = series(Series::geometric, min, max);
//        System.out.println(sum);
//
        double sum = series(n -> Math.pow(0.25, n), min, max);
        System.out.println(sum);
    }

    private static double geometric(int n) {
        return Math.pow(1.0 / 4.0, n);
    }
    
    private static Double series(IntToDoubleFunction fn, 
            int low, int high) {
        return IntStream.range(low, high).mapToDouble(fn).sum();
    }

//// generic approach, with 'import java.util.function.Function;'
//    private static Double series(Function<Integer, Double> fn, 
//            int low, int high) {
//        return IntStream.range(low, high)
//                        .mapToDouble(n -> fn.apply(n))
//                        .sum();
//    }

}
