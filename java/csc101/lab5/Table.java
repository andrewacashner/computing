/* # Table
 *
 * Output a formatted table of the cost of flooring materials per square foot
 * for every width and length combination within a preset range.
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC 101, Lab 5
 * | 2024/10/03
 */

import java.util.Scanner;

class Table {
   public static void main(String[] args) {
      // Constants
      final int MIN_WIDTH_FEET = 1;
      final int MAX_WIDTH_FEET = 8;

      final int MIN_LENGTH_FEET = 1;
      final int MAX_LENGTH_FEET = 8;

      // Welcome, Get configuration info from user
      System.out.println("FLOORING MATERIALS");
      System.out.print("Enter the cost per square foot (USD): $");

      Scanner kbScan = new Scanner(System.in);
      double costPerSqFtUSD = kbScan.nextDouble();

      // Print table: Start with minimum value minus one in order to add header labels
      //    Rows
      for (int widthFeet = MIN_WIDTH_FEET - 1;
            widthFeet <= MAX_WIDTH_FEET;
            ++widthFeet) {

         if (widthFeet == MIN_WIDTH_FEET - 1) { // Header
            System.out.printf("%10s", "");
         } else {
            System.out.printf("%10d", widthFeet);
         }

         // Columns
         for (int lengthFeet = MIN_LENGTH_FEET;
               lengthFeet <= MAX_LENGTH_FEET;
               ++lengthFeet) {

            if (widthFeet == MIN_WIDTH_FEET - 1) { // Header
               System.out.printf("%10d", lengthFeet);
            } else {
               double thisCost = widthFeet * lengthFeet * costPerSqFtUSD;
               System.out.printf("%10s", String.format("$%.2f", thisCost));
            }
         }

         System.out.println();
      }
   }
}
