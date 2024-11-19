import java.util.function.Function;

public class SeriesImperative {
    public static void main (String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: Series MIN MAX");
            return;
        }
        int min = Integer.parseInt(args[0]);
        int max = Integer.parseInt(args[1]);
        
        double sum = series(SeriesImperative::geometric, min, max);
        System.out.println(sum);
    }

    private static double geometric(int n) {
        return Math.pow(0.25, n);
    }
    
    private static double series(Function<Integer, Double> fn, 
            int low, int high) {
        double sum = 0;
        for (int i = low; i < high; ++i) {
            sum += fn.apply(i);
        }
        return sum;
    }
}
