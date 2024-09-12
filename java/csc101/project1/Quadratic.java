/* Quadratic
 *
 * Solve a quadratic equation of the form ax^2 + bx + c = 0,
 * given inputs a, b, and c from the user.
 *
 * Andrew A. Cashner, 2024/09/10
 * For CS101, Assignment 1
 */

import java.util.Scanner;

class Quadratic {

    public static void main(String[] args) {
        // Greeting, instructions
        System.out.println("QUADRATIC EQUATION SOLVER");
        System.out.println("Solve any equation of the form ax^2 + bx + c.");
        System.out.println("Please enter three numbers for a, b, and c:");

        // Read input
        Scanner input = new Scanner(System.in);
        double coefA = input.nextDouble();
        double coefB = input.nextDouble();
        double coefC = input.nextDouble();

        // Compute result
        double root = Math.sqrt(Math.pow(coefB, 2) - 4 * coefA * coefC);
        double denominator = 2 * coefA;
        double resultPositive = (-coefB + root) / denominator;
        double resultNegative = (-coefB - root) / denominator;

        // Return output
        System.out.printf(
                "\nIf %.2fx^2 + %.2fx + %.2f = 0,\nthen x = %.2f or %.2f\n", 
                coefA, coefB, coefC, resultPositive, resultNegative);
    }
}
