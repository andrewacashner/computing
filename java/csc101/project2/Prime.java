/* Prime
 * Find prime numbers within a given range
 *
 * Andrew Cashner, `acashner@student.monroecc.edu`
 * CSC101, Project 2
 * 2024/10/01
 */

import java.util.Scanner;

class Prime {
   public static void main(String[] args) {
      // Welcome
      System.out.println("PRIME NUMBER FINDER");
      System.out.println("Find prime numbers between a given minimum and maximum (inclusive)");

      // Get minimum and maximum values for range
      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter the minimum: ");
      int min = kbScan.nextInt();

      System.out.print("Enter the maximum: ");
      int max = kbScan.nextInt();

      // Validate input
      //   - The range must extend into the positive range
      //   - The user can enter a minimum below 2 but we will actually
      //     start at 2 since 0 and 1 are not primes and negatives are
      //     impossible
      //   - Therefore we check if max is greater than min and greater than 2
      boolean isValidInput = max >= 2 && max > min;
      if (!isValidInput) {
          System.err.println("Invalid range");
          System.exit(1);
      }

      // Reset minimum if needed: 0 and 1 are not primes
      int realMin = min > 2 ? min : 2;

      // Calculate primes and print results as we go; count primes found
      System.out.print("Primes: ");
      int primeCount = 0; 
      
      // Check every integer value between min and max
      // For each one, check for divisors between 2 and itself - 1; if any
      // divide evenly, this number is not prime.

      for (int current = realMin; current <= max; ++current) {

         boolean hasFactor = false;
         for (int i = 2; !hasFactor && i < current; ++i) {
             hasFactor = current % i == 0;
         }

         if (!hasFactor) {
            System.out.printf("%d ", current);
            ++primeCount;
         }
      }

      // Report number of primes found
      String plural = primeCount == 1 ? "" : "s";

      System.out.printf("\nFound %d prime number%s between %d and %d.\n", 
            primeCount, plural, min, max);
      System.out.println("Until next prime...");
   }
}
