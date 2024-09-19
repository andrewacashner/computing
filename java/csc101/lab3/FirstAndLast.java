/* First and Last Substrings
 *
 * Given two strings and an integer n, return substrings of the first n
 * characters of the first string, and the last n characters of the second
 * string.
 * (If the limit is longer than either string, return the whole string.)
 *
 * CSC101, Lab 3
 * 2024/09/19
 * Andrew Cashner, acashner@student.monroecc.edu
 */

import java.util.Scanner;

class FirstAndLast {
   public static void main(String[] args) {
      System.out.println("FIRST AND LAST SUBSTRING");
      System.out.println("Given two strings and a number n, extract the first n characters of the first string and the last n characters of the second.\n");

      // Get user input
      Scanner kbScan = new Scanner(System.in);

      System.out.println("Enter a string of any length (Press Return/Enter at the end of the string):");
      String strOne = kbScan.nextLine();

      System.out.println("Enter another string:");
      String strTwo = kbScan.nextLine();

      System.out.println("Enter the number of characters you want to extract (spaces count as characters): ");
      int limit = kbScan.nextInt();

      // Extract the strings
      String strOneTrimmed, strTwoTrimmed;

      // If the limit is longer than either string, return the whole string
      if (limit > strOne.length()) {
         strOneTrimmed = strOne;
      } else {
         // First n characters
         strOneTrimmed = strOne.substring(0, limit);
      }

      if (limit > strTwo.length()) {
         strTwoTrimmed = strTwo;
      } else {
         // Last n characters
         strTwoTrimmed = strTwo.substring(strTwo.length() - limit);
      }

      // Print the output
      System.out.printf("\nFirst %d characters of string \"%s\" => \"%s\"\n", limit, strOne, strOneTrimmed);

      System.out.printf("Last %d characters of string \"%s\" => \"%s\"\n", limit, strTwo, strTwoTrimmed);
   }
}
