import java.util.Scanner;

/**
 * Reverse a given string with the character cases reversed.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/17 (CSC 101, Lab 7)
 */
public class ReverseCase {
    /**
     * Get an input line, reverse the string, print the result.
     *
     * @param args (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        Scanner kbScan = new Scanner(System.in);
        
        System.out.println("CASE REVERSER");
        System.out.println("Enter a line of text:");
        String input = kbScan.nextLine();

        System.out.println(swapCase(input));
    }

    /**
     * Reverse the case of each character in a string.
     *
     * @param source Input string
     * @return String with swapped case
     */
    public static String swapCase(String source) {
        StringBuilder inverted = new StringBuilder();

        for (int i = 0; i < source.length(); ++i) {
            char thisChar = source.charAt(i);
            char newChar;

            if (Character.isUpperCase(thisChar)) {
                newChar = Character.toLowerCase(thisChar);
            } else {
                newChar = Character.toUpperCase(thisChar);
            }
            inverted.append(newChar);
        }

        return inverted.toString();
    }
}
