/* Quadratic
 *
 * Solve a quadratic equation of the form ax^2 + bx + c = 0,
 * given inputs a, b, and c from the user.
 *
 * Bonus-ified version for play
 *
 * Andrew A. Cashner, 2024/09/10
 * For CS101, Assignment 1
 */

import java.util.Scanner;

class QuadraticEquation {
    double coefA, coefB, coefC;
    double positive, negative;
    
    final double NEGATIVE = -1.0;
    final double POSITIVE = 1.0;

    double formula(double coefB, double discriminant, 
            double denominator, double range) {
        return (-coefB + range * Math.sqrt(discriminant)) / denominator;
    }

    public QuadraticEquation(double coefA, double coefB, double coefC) {
        coefA = coefA;
        coefB = coefB;
        coefC = coefC;

        double discriminant = Math.pow(coefB, 2) - 4 * coefA * coefC;
        if (discriminant < 0) {
            throw new Error("Cannot compute: Complex result, square root of a negative number");
            // TODO calculate complex result
        }

        double denominator = 2 * coefA;
        if (denominator == 0) {
            throw new Error("Cannot compute: Division by zero");
        }

        positive = formula(coefB, discriminant, denominator, POSITIVE);
        negative = formula(coefB, discriminant, denominator, NEGATIVE);
    }

    String format(double num) {
        return String.format("%.2f", num);
    }

    public String resultString() {
        return String.format("%s or %s", format(positive), format(negative));
    }

    public String explanation() {
        return String.format("If %sx^2 + %sx + %s = 0, then x = %s.", format(coefA), format(coefB), format(coefC), resultString());
    }

}

class Quadratic {

    public static void main(String[] args) {
        // Greeting, instructions
        System.out.println("Quadratic Equation Solver");
        System.out.println("I can solve any equation of the form ax^2 + bx + c.");
        System.out.println("Please enter three numbers for a, b, and c:");

        // Read input
        Scanner input = new Scanner(System.in);
        double first = input.nextDouble();
        double second = input.nextDouble();
        double third = input.nextDouble();

        try {
            QuadraticEquation equation = new QuadraticEquation(first, second, third);
            System.out.println(equation.explanation());
        } 
        catch(Error e) {
            System.out.println(e);
        }
    }
}
