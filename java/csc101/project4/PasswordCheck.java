import java.util.Scanner;

/**
 * Password checker
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/25 (CSC 101, Project 4)
 */
public class PasswordCheck {
    // Valid password:
    //  1. At least 10 characters, not more than 16
    //  2. Contains at least one of these symbols: 
    //      !, #, $, %, ^, &, *, <, >, ?
    //  3. Contains at least two capital letters
    //  4. Contains exactly one digit
    //  5. Contains no spaces
    //
    // User inputs password, program says if it is valid; if not report which
    // conditions it fails.
    // Loop to test multiple passwords until user exits.
    public static void main(String[] args) {
        Scanner kbScan = new Scanner(System.in);

        System.out.println("PASSWORD CHECKER");
        String input = new String();

        while (input != "q") {
            System.out.print("Enter a password to test (or 'q' to quit): ");
            String password = kbScan.nextLine();
            if (isValid(password)) {
                // say so
            } else {
                // say so
                // report failure points
            }
        }

        System.out.println("Thank you for using the Password Checker.");
    }

    static boolean validContentLength(String password) {
        int length = password.length();
        return length >= 10 && length <= 16;
    }

    static boolean validContentCapitals(String password) {
        int capitalCount = 0;
        for (int i = 0; i < password.length(); ++i) {
            if (Character.isUpperCase(password.charAt(i))) {
                ++capitalCount;
            }
        }
        return capitalCount;
    }

    static boolean contains(String set, char c) {
        boolean found = false;
        for (int i = 0; i < set.length() && !found; ++i) {
            found = set.charAt(i) == c;
        }
        return found;
    }

    static boolean containsAtLeast(String str, String matchSet, 
            int minMatches) {

        int foundMatches = 0;
        for (int i = 0; i < str.length(); ++i) {
            if (contains(matchSet, str.charAt(i))) {
                ++foundMatches;
            }
        }
        return foundMatches >= minMatches;
    }

    static boolean validContentSymbols(String password) {
        String symbols = new String("!#$%^&*<>?");
        return containsAtLeast(password, symbols, 1);
    }

    static boolean validContentDigits(String password) {
        String digits = "0123456789";
        return containsAtLeast(digits, 1);
    }

    static boolean validContentSpaces(String password) {
        return password.indexOf(' ') == -1;
    }
}
