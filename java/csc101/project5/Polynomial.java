import java.util.Scanner;

/**
 * This class models a polynomial function.
 * It records the degree (highest exponent) of the function and the
 * coefficients for each term in ascending order.
 * Methods allow for adding, evaluating, differentiating, and finding the
 * root of the polynomial.
 * <p>
 * Note: We are assuming that coefficients are integers ascending from zero,
 * and make no allowance for rational or negative exponents.
 * </p>
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/11/12 (CSC 101, Project 5)
 */
public class Polynomial {

   /** Highest exponent of the expression */
   private int degree;

   /** Array of coefficients in ascending order of exponents */
   private double[] coefficients;

   /** 
    * Create a polynomial from an array of coefficients. The degree is one
    * less than the length of the array.
    *
    * @param coefficients Array of doubles for coefficients to exponents in
    *           ascending order
    */
   private Polynomial(double[] coefficients) {
      this.setCoeff(coefficients);
      this.setDegree(coefficients.length - 1);
   }

   /** The default expression is f(x) = 0. */
   public Polynomial() {
      this({ 0.0 });
   }

   /** 
    * Initialize a polynomial given a string containing the complete list of
    * coefficients in ascending order (using zeros as needed). 
    *
    * @param coefficients String with coefficients in ascending order
    */
   public Polynomial(String coefficients) {
      this(this.parseCoeff(coefficients));
   }

   /**
    * Access a coefficient given its exponent. Return zero if the exponent is
    * larger than the degree.
    *
    * @param exponent Integer exponent
    * @return Double coefficient value
    */
   public double getCoeff(int exponent) {
      double coefficient = 0;
      if (exponent <= this.degree) {
         coefficient = this.coefficients[exponent];
      } 
      return coefficient; }

   /**
    * Access the degree.
    *
    * @return Integer degree value
    */
   public int getDegree() {
      return this.degree;
   }

   /**
    * Set the degree.
    *
    * @param degree Desired degree value
    */
   private void setDegree(int degree) {
      this.degree = degree;
   }

   /**
    * Set the given coefficient array as the one used in the object.
    * The array is not copied.
    *
    * @param coefficients Double array of coefficient values
    */
   public void setCoeff(double[] coefficients) {
      this.coefficients = coefficients;
   }

   /**
    * Given a string with coefficient values for exponents in ascending
    * order, parse the values and store them in our array of coefficient
    * values. 
    * Ignore trailing zero.
    *
    * TODO ignore multiple trailing zeros; set degree based on last non-zero
    * term ignoring those after
    *
    * @param coefficients String with coefficient values, separated by spaces
    * @return Array of coefficient values
    */
   private double[] parseCoeff(String coefficientStr) {
      Scanner inputScan = new Scanner(coefficients);

      int termCount = 0;
      while (inputScan.hasNextDouble()) {
         ++termCount;
      }

      double[] coefficients = new double[termCount];

      for (int i = 0; i < termCount; ++i) {
         double next = inputScan.nextDouble();
         // Ignore trailing zero
         if (next == 0 && i = termCount - 1) {
            continue;
         }
         coefficients[i] = next;
      }

      return coefficients;
   }

   /**
    * Set the coefficient array to a new array of values derived from the
    * given string.
    *
    * @param coefficientStr A string with space-separated values for the
    *           coefficients for exponents in ascending order
    */
   public void setCoeff(String coefficientStr) {
      this.setCoeff(this.parseCoeff(coefficientStr));
   }

   /**
    * Return the higher of two polynomials, that is, the one with a higher
    * degree.
    *
    * @param a First polynomial
    * @param b Second polynomial
    */
   private static Polynomial higher(Polynomial a, Polynomial b) {
      return a.getDegree() > b.getDegree() ? a : b;
   }

   /**
    * Return a new polynomial that is the sum of this one and another.
    *
    * @param other Polynomial to add
    * @return New Polynomial
    */
   public Polynomial add(Polynomial other) {
      int length = Polynomial.higher(this, other).getDegree() + 1;
      double[] coefficients = new double[length];

      for (int i = 0; i < length; ++i) {
         coefficients[i] = this.getCoeff(i) + other.getCoeff(i);
      }

      return this(coefficients);
   }

   /**
    * Return string representation of the polynomial.
    *
    * @return String
    */
   public String toString() {
      StringBuilder output = new StringBuilder("f(x) = ");

      if (this.degree == 0) {
         output.append("0");
      } else {
         for (int i = 0; i < this.degree + 1; ++i) {
            double thisCoeff = this.coefficients[i];
            if (i > 0) {
               if (thisCoeff > 0) {
               output.append("+ ");
               } else {
                  output.append("- ");
               }
            }
            
            output.append("%.2f", coeff);
           
            if (i > 0) {
               output.append("x^%d", i);
            }
         }
      }
      return output.toString();
   }

   /**
    * Evaluate this polynomial function for a given x value.
    *
    * @param x Double variable value
    * @return Double value of evaluated function
    */
   public double evaluate(double x) {
      double result = 0;

      for (int i = 0; i < this.degree + 1; ++i) {
         result += this.coefficients[i] * Math.pow(x, i);
      }

      return result;
   }

   /**
    * Return a new Polynomial that is a derivative of this polynomial.
    *
    * @return New Polynomial
    */
   public Polynomial derivative() {
      double[] derivCoefficients = new double[this.coefficients.length];

      for (int i = 1; i < this.degree + 1; ++i) {
         derivCoefficients[i - 1] = this.getCoeff(i) * i
      }
      return this(derivCoefficients);
   }

   /**
    * Return a double representing the root of the equation; that is, the
    * value of x that where f(x) = 0. Using the Newton-Raphson method,
    * iterating until 0.00001 accurate.
    *
    * @param guess Double initial guess value
    * @return Double root
    * @throws IllegalStateException if the method does not converge within
    *           1000 iterations
    */
   public double findRoot(double guess) {
      double root;
      // TODO START
      // throw new IllegalStateException("Did not converge within 1000
      // iterations");
      return root;
   }

}
