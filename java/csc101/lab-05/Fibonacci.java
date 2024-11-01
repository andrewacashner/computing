/* # Fibonacci
 *
 * For a given n > 1, output the first n Fibonnaci numbers.
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC 101, Lab 5
 * | 2024/10/03
 */

import java.util.Scanner;

class Fibonacci {
   public static void main(String[] args) {
      // Welcome, Get user configuration
      System.out.println("FIBONACCI");

      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter number of Fibonacci numbers to compute (minimum 2, maximum 80): ");
      int max = kbScan.nextInt();

      // Set range in safe limits
      if (max < 2) {
         System.out.println("Too low: Resetting to minimum of 2");
         max = 2;
      } else if (max > 80) {
         System.out.println("Too high: Resetting to maximum of 80");
         max = 80;
      }

      // We know the first two and there is a minimum of two outputs
      System.out.print("1, 1");

      long prev = 1;
      long current = 1;

      // Calculate numbers and print results as we go
      for (int i = 2; i < max; ++i) {
         long next = prev + current;
         prev = current;
         current = next;

         if (i < max) {
            System.out.printf(", ");
         }
         System.out.printf("%d", current);
      }
      System.out.println();
   }
}
