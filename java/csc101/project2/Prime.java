/* # Prime
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC101, Project 2
 * | 2024/10/08
 *
 * ## Description
 *
 * Find prime numbers within a given range.
 * The user inputs a minimum and a maximum and the program finds all prime
 * numbers between those numbers (inclusive).
 */

import java.util.Scanner;

class Prime {
   public static void main(String[] args) {
      // Welcome
      System.out.print("PRIME NUMBER FINDER\n");
      System.out.print("Find prime numbers between a given minimum and maximum (inclusive)\n\n");

      // Get minimum and maximum values for range
      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter the minimum: ");
      int min = kbScan.nextInt();

      System.out.print("Enter the maximum: ");
      int max = kbScan.nextInt();

      exitIfInputInvalid(min, max);

      // Calculate primes and print results as we go; count primes found
      // - Check every integer value between min and max
      // - For each one, check for divisors between 2 and itself - 1; if any
      //    divide evenly, this number is not prime.
      int primeCount = 0; 
      String primes = new String();

      for (int current = validMinimum(min); current <= max; ++current) {
         if (isPrime(current)) {
            primes += String.format("%d ", current);
            ++primeCount;
         }
      }

      System.out.printf("\nPrimes: %s\n", primes);
      System.out.print(primeReport(primeCount, min, max));
      System.out.println("Until next prime...");
   }

   // Validate input
   //   - The range must extend into the positive range
   //   - The user can enter a minimum below 2 but we will actually
   //     start at 2 since 0 and 1 are not primes and negatives are
   //     impossible
   //   - Therefore we check if max is greater than min and greater than 2
   public static void exitIfInputInvalid(int min, int max) {
      boolean isValidInput = max >= 2 && max > min;
      if (!isValidInput) {
         System.err.println("Invalid range");
         System.exit(1);
      }
   }

   // Reset minimum if needed: 0 and 1 are not primes
   public static int validMinimum(int min) {
      return min > 2 ? min : 2;
   }

   // Does the given integer have any factors other than 1 and itself?
   // If not, it is prime
   public static boolean isPrime(int value) {
      boolean hasFactor = false;
      for (int i = 2; !hasFactor && i < value; ++i) {
         hasFactor = value % i == 0;
      }
      return !hasFactor;
   }

   // Report number of primes found
   public static String primeReport(int primeCount, int min, int max) {

      String plural = primeCount == 1 ? "" : "s";

      return String.format("Found %d prime number%s between %d and %d.\n", 
            primeCount, plural, min, max);
   }
}
