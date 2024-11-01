import java.util.Scanner;

/** 
 * Test of the EstimateMath library for approximate math.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/10 (CSC 101, Lab 6)
 */
public class MathDriver {
   /**
    * Get two integers from input and report their estimated sum and
    * difference. Loop until user enters two zeros.
    *
    * @param args (Unused) command-line arguments
    */
   public static void main(String[] args) {
      System.out.println("Let's do inaccurate math!");
      Scanner kbScan = new Scanner(System.in);

      int numA, numB;

      do {
         System.out.print("Enter two integers (or 0 0 to quit): ");
         numA = kbScan.nextInt();
         numB = kbScan.nextInt();
         kbScan.nextLine(); // Clear buffer

         if (!bothZero(numA, numB)) {
            int sum = EstimateMath.estimateAdd(numA, numB);
            int diff = EstimateMath.estimateSubtract(numA, numB);

            // Approximately equal sign is U+2248
            System.out.printf("\n%d + %d ≈ %d\n", numA, numB, sum);
            System.out.printf("%d - %d ≈ %d\n\n", numA, numB, diff);
         }

      } while (!bothZero(numA, numB));

      System.out.println("\nWarning: Do not rely on the results of this program!");
   }


   /** Are both integers zero? */
   static boolean bothZero(int numA, int numB) {
      return numA == 0 && numB == 0;
   }
}
