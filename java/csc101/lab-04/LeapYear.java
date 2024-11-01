/* LEAP YEAR
 *
 * Given a single integer representing a year, output whether or not that
 * year is a leap year. We calculate the leap year with this algorithm:
 *
 *    1. If the year is evenly divisible by 4, go to step 2. Otherwise, go to
 *       step 5.
 *    2. If the year is evenly divisble by 100, go to step 3. Otherwise, go
 *       to step 4.
 *    3. If the year is evenly divisble by 400, go to step 4. Otherwise, go
 *       to step 5.
 *    4. The year is a leap year (it has 366 days).
 *    5. The year is not a leap year (it has 365 days).
 *
 * We are assuming valid input. 
 *
 * CSC 101, Lab 4
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/26
 */

import java.util.Scanner;

class LeapYear {
   public static void main(String[] args) {

      // Welcome, get input
      System.out.println("LEAP YEAR CALCULATOR");

      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter a year to find out if it is a leap year: ");

      int year = kbScan.nextInt();

      // Calculate
      boolean divBy4 = year % 4 == 0;
      boolean divBy100 = year % 100 == 0;
      boolean divBy400 = year % 400 == 0;

      /* // We could do it this way with if/else but since we are just
       * // setting a flag based on conditions it seems more elegant just
       * // to do that directly.
       * boolean isLeapYear = false;
       * if (divBy4) {
       *     if (divBy100 && divBy400) {
       *         isLeapYear = true;
       *     } else {
       *         isLeapYear = true;
       *     }
       * } 
       */
      boolean isLeapYear = 
         divBy4 && divBy100 && divBy400
         || divBy4 && !divBy100;

      // Output result
      if (isLeapYear) {
         System.out.printf("%d is a leap year.\n", year);
      } else {
         System.out.printf("%d is not a leap year.\n", year);
      }


   }
}
