/* Pick A Word
 */

import java.util.Scanner;

/** 
 * Given a sentence and an integer n, return word n of the sentence.
 *
 * <ul>
 *   <li>If n &lt;= 0, return an empty string.</li>
 *   <li>If n &gt; length of string, return the last word.</li>
 * </ul>
 *
 * Repeat the main function in a loop until user quits.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/09 (CSC 101, Project 3)
 */
public class PickWord {
    /** 
     * Get user input, find and report selected word; loop until user quits.
     * @param args (Unused) command-line arguments
     */
    public static void main(String[] args) {
        // Greeting
        System.out.print("PICK A WORD\n");
        System.out.print("Extract a word from a string at a chosen position\n\n");

        Scanner kbScan = new Scanner(System.in);
        String input = new String("y");

        while (input.equals("y")) {
            // Get input
            System.out.print("Enter a multiple-word phrase: ");
            String source = kbScan.nextLine();

            System.out.print("Enter the number of the word to pick: ");
            int position = kbScan.nextInt();
            kbScan.nextLine(); // Skip the newline entered after the number

            // Find and report word
            String selection = nthWord(source, position);
            System.out.printf("Found word: \"%s\"\n", selection);

            // Test for loop end
            System.out.print("\nGo again? Press y (or any other key to quit): ");
            input = kbScan.nextLine();
        }

        // Closing
        System.out.print("Thank you for using Pick a Word.\n");
    }

    /** 
     * Choose the nth word of a string (1-indexed).
     * Words are considered separated by a single space character.
     * For position &lt;= 0, return empty string; for position &gt; string
     * length, return last word.
     *
     * @param source String to parse
     * @param position 1-indexed position of desired word
     *
     * @return The selected word
     */
    public static String nthWord(String source, int position) {

        String selection = new String("");

        if (position > 0) {
            int start = 0;
            int end = 0;

            // Mark the end of space-delimited words until we have found the
            // word at the desired position.
            for (int i = 0, wordCount = 0; 
                    i < source.length() && wordCount < position;
                    ++i) {

                if (source.charAt(i) == ' ') {
                    ++wordCount;

                    // Did we just find the nth word? If so, i will be the
                    // end position of the word. If not, mark the start of
                    // the next word as the next candidate.
                    if (wordCount < position) {
                        start = i + 1;
                    }
                }
                end = i;
            }

            // If the desired position was larger than the number of words,
            // we ran out at the end of the string without finding a string;
            // so we need to shift end one to the right to get the rest.
            if (end == source.length() - 1) {
                ++end;
            }

            selection = source.substring(start, end);
        }

        return selection;
    }
}
