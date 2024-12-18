/** 
 * Sum of digits for all integers within a range
 * @author Andrew Cashner
 * @version 2024/10/18
 */
public class SumDigits {
    public static void main(String[] args) {
        int sum = sumDigits();
        System.out.println(sum);
    }

    private static int sumDigits() {
        int sum = 0;
        for (int i = 0; i < 10; ++i) {
            sum += 5 * (9 + 2 * i);
        }
        ++sum; // one more for 100
        return sum;
    }
}
