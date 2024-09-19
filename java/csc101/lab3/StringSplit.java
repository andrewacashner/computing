/* String Split
 *
 * Given a string containing the character '!', split the string into two at
 * the location of that character and print the two parts on separate lines.
 *
 * CSC 101, Lab 3
 * 2024/09/19
 * Andrew Cashner, acashner@student.monroecc.edu
 */

import java.util.Scanner;

class StringSplit {
   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);

      // Welcome
      System.out.println("STRING SPLITTER");
      System.out.println("Split a string into two parts at the '!' character");

      // Get user input
      System.out.println("Enter a string of at least 10 characters, containing a '!' (no spaces):");
      String input = kbScan.next();

      // Validate input: If invalid, report the problem and exit
      if (input.length() < 10) {
         System.out.println("The string must be longer than 10 characters.");
      } else {
         int index = input.indexOf('!');
         if (index == -1) {
            System.out.println("The string must contain the character '!' to mark the split point");
         } else {

            // Split the string
            String head = input.substring(0, index);
            String tail = input.substring(index + 1);

            // Show output
            System.out.printf("\nThe string \"%s\" splits into \"%s\" and \"%s\".\n", input, head, tail);
         }
      }
   }
}
