import java.util.Scanner;

/**
 * Test if a given word is a palindrome.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/17 (CSC 101, Lab 7)
 */

public class Palindrome {
    /**
     * Ask for an input line, check if it is a palindrome, and report the
     * result.
     *
     * @param args (unused) Command-line arguments
     */
    public static void main(String[] args) {
        Scanner kbScan = new Scanner(System.in);

        System.out.println("PALINDROME CHECKER");
        System.out.print("Enter a possible palindrome: ");
        String input = kbScan.nextLine();

        String result = isPalindrome(input) ? "IS" : "IS NOT";
        System.out.printf("\"%s\" %s a palindrome.\n", input, result);
    }

    /**
     * Is a given string a palindrome? That is, is it the same forward and
     * backward?
     *
     * (Single-character strings are not palindromes.)
     *
     * @param source Input string
     * @return Boolean: True if it is a palindrome
     */
    public static boolean isPalindrome(String source) {
        int end = source.length() - 1;
        if (source.length() == 1) {
            return false; 
        } else {
            boolean isPalindrome = true;

            for (int i = 0; i < source.length() / 2 && isPalindrome; ++i) {
                isPalindrome = source.charAt(i) == source.charAt(end - i);
            }
            return isPalindrome;
        }
    }
}

