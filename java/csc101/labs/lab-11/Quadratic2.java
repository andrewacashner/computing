import java.util.Scanner;
import java.util.InputMismatchException;

/** 
 * Solve a quadratic equation of the form ax^2 + bx + c = 0,
 * given inputs a, b, and c from the user.
 * Now with exception handling.
 *
 * @author Andrew A. Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/09/21 (CSC101, Lab 11)
 */
public class Quadratic2 {

   /**
    * Ask for three inputs and validate each input.
    * Compute the roots or report invalid inputs.
    *
    * @param args Unused command-line arguments
    */
   public static void main(String[] args) {
      System.out.println("QUADRATIC EQUATION SOLVER");
      System.out.println("Solve any equation of the form ax^2 + bx + c.");

      Scanner kbScan = new Scanner(System.in);
      double[] coefficients = new double[3];

      double[] roots = null;

      while (roots == null) {
         try {
            System.out.println("Please enter three numbers for a, b, and c:");
            for (int i = 0; i < coefficients.length; ++i) {
               try {
                  coefficients[i] = kbScan.nextDouble();

               } catch (InputMismatchException e) {
                  System.err.printf("Invalid input \"%s\": Try again\n\n", 
                        kbScan.nextLine());
                  --i;
               }
            }
            roots = quadraticSolver(coefficients);

         } catch (IllegalArgumentException e) {
            System.err.print(e.getMessage() + "; Try again\n\n");
         }
      }

      System.out.printf(
            "\nIf %.2fx^2 + %.2fx + %.2f = 0,\nthen x = %.2f or %.2f\n", 
            coefficients[0], coefficients[1], coefficients[2],
            roots[0], roots[1]);
   }

   /**
    * Return an array containing the positive and negative roots of a
    * quadratic equation with the given coefficients.
    *
    * @param coefficients Array of doubles with coefficient values for
    *       exponents in descending order
    * @return Two-element array with positive and negative roots
    * @throws IllegalArgumentException if first coefficient is zero or if
    *       discriminant is negative
    */
   public static double[] quadraticSolver(double[] coefficients) 
         throws IllegalArgumentException {

         double coefA = coefficients[0];
         double coefB = coefficients[1];
         double coefC = coefficients[2];
         
         if (coefA == 0) {
            throw new IllegalArgumentException("Cannot compute: The first coefficient cannot equal zero");
         }

         double discriminant = Math.pow(coefB, 2) - 4 * coefA * coefC;

         if (discriminant < 0) {
            throw new IllegalArgumentException("Cannot compute: The discriminant cannot be negative");
         }

         double root = Math.sqrt(discriminant);
         double denominator = 2 * coefA;
         double resultPositive = (-coefB + root) / denominator;
         double resultNegative = (-coefB - root) / denominator;

         double[] solutions = { resultPositive, resultNegative };
         return solutions;
   }
}
