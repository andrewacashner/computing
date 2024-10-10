/* FYI I am also trying to learn javadoc documentation. */

import java.util.Scanner;

/** 
 * Report how many odd digits are in an integer.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/10 (CSC 101, Lab 6)
 */
public class OddDigit {
    /**
     * Get an integer from input, count the digits, and report the results.
     * Loop until user enters '0'.
     *
     * @param args (Unused) command-line arguments
     */
    public static void main(String[] args) {
        System.out.print("ODD DIGIT COUNTER\n");
        System.out.print("Count the odd digits in an integer.\n\n");

        Scanner kbScan = new Scanner(System.in);
        int inputNum = 1;

        while (inputNum != 0) {
            // Get input
            System.out.print("Enter an integer (or 0 to quit): ");
            inputNum = kbScan.nextInt();
            kbScan.nextLine(); // Clear buffer
        
            if (inputNum != 0) {
                // Count and report odd digits
                int odds = oddDigits(inputNum);
                System.out.printf("\nThe integer %d contains %d odd %s.\n", 
                        inputNum, odds, maybePlural("digit", odds));
            }
        }
    }

    /**
     * Is this an odd number?
     *
     * @param num The number to test
     * @return Boolean value, true if odd
     */
    public static boolean isOdd(int num) {
        return num % 2 != 0;
    }

    /** 
     * Count how many odd digits are in an integer.
     *
     * @param num The integer to examine
     * @return The number of odd digits
     */
    public static int oddDigits(int num) {
        int oddCount;

        for (oddCount = 0; num > 0; num /= 10) {
            int digit = num % 10;
            if (isOdd(digit)) {
                ++oddCount;
            }
        }

        return oddCount;
    }

    /**
     * Add a plural 's' to a noun if the quantity of that item is more than
     * one.
     *
     * @param noun The item
     * @param quantity The number of that item
     * @return The given noun string with or without an added suffix 's'
     */
    public static String maybePlural(String noun, int quantity) {
        String suffix = quantity == 1 ? "" : "s";
        return noun + suffix;
    }
}
