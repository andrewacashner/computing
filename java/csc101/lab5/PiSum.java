/* # PiSum
 *
 * Calculate pi using an infinite summation of terms.
 * End the summation when the difference between successive values drops
 * below 0.000001.
 *
 * The formula is as follows:
 *
 * Sum = 4/1 - 4/3 + 4/5 - 4/7 ...
 *
 * Track number of loops used and output the results to the requested
 * precision.
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC 101, Lab 5
 * | 2024/10/03
 */

import java.util.Scanner;

class PiSum {
   public static void main(String[] args) {
      System.out.println("PI CALCULATOR");
      System.out.println("Use a summation formula to calculate pi up to the desired accuracy\n");

      // Set accuracy (decimal places)
      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter the number of decimal places to find (maximum of 10): ");
      int decimalPlaces = kbScan.nextInt();

      if (decimalPlaces > 10) {
         System.out.printf("That's beyond my capacity: Resetting precision to 10");
         decimalPlaces = 10;
      }

      double minDiff = 1 * Math.pow(10, -decimalPlaces);

      // Calculate sum
      double diff;
      int factor = 1;
      long loopCount = 0;
      
      double prevPi = 0;
      double denominator = 1;
      double currentPi = 4 / denominator;

      for (denominator = 3; 
            Math.abs(currentPi - prevPi) > minDiff; 
            denominator += 2) {

         factor = -factor;
         prevPi = currentPi;
         currentPi += factor * 4 / denominator;
         ++loopCount;
      }

      // Print results
      System.out.printf("\nCalculated the value of pi in %,d iterations\n", loopCount);

      // Display the number of decimal places requested
      String piValue = String.format("%." + decimalPlaces + "f", currentPi);
      System.out.printf("pi ≈ %s\n", piValue);
   }
}
