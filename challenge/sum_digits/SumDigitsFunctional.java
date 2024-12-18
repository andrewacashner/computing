import java.util.stream.*;

/** 
 * Sum of digits for all integers within a range
 * @author Andrew Cashner
 * @version 2024/10/18
 */
public class SumDigitsFunctional {
    public static void main(String[] args) {
        int sum = sumDigits();
        System.out.println(sum);
    }

    private static int sumDigits() {
        int sum = IntStream.range(0, 10)
                    .reduce(0, (acc, n) -> acc + 5 * (9 + 2 * n));
        return sum + 1;
    }
}
