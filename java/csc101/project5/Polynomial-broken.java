import java.util.Arrays;

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

   /** For testing */
   public static void main(String[] args) {
      if (args.length < 1) {
         System.err.println("Usage: java Polynomial \"c0 c1 c2 c3 ...\"");
         return;
      }
      
     String input = args[0];
     Polynomial fn = new Polynomial(input);
     System.out.println(fn);

     // for (double x = 0; x < 5.0; ++x) {
     //    System.out.printf("f(%.0f) = %.4f\n", x, fn.evaluate(x));
     // }

      Polynomial a = new Polynomial("1 2 3");
      System.out.println("Degree: " + a.getDegree());
      System.out.println(Arrays.toString(a.coefficients));
      System.out.println(a);
      
      Polynomial b = new Polynomial("1 1 1 1");
      System.out.println(b);
      Polynomial c = a.add(b);
      System.out.println(c);

      Polynomial d = fn.derivative();
      System.out.println(d);
   }

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
      System.out.printf("Called private constructor with double array %s\n", Arrays.toString(coefficients));
   }

   /** The default expression is f(x) = 0. */
   public Polynomial() {
      this(new double[1]);
   }

   /** 
    * Initialize a polynomial given a string containing the complete list of
    * coefficients in ascending order (using zeros as needed). 
    *
    * @param coefficients String with coefficients in ascending order
    */
   public Polynomial(String coefficients) {
      this(Polynomial.parseCoeff(coefficients));
      double[] coeff = Polynomial.parseCoeff(coefficients);
      System.out.printf("Create polynomial from array %s\n", Arrays.toString(coeff));
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
      System.out.printf("Try to get coeff for exp %d\n", exponent);
//      if (exponent <= this.getDegree()) {
      if (exponent < this.coefficients.length - 1) {
         coefficient = this.coefficients[exponent];
      } 
      return coefficient; 
   }

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
   private void setCoeff(double[] coefficients) {
      System.out.printf("setCoeff");
      this.coefficients = Polynomial.stripTrailingZeros(coefficients);
      System.out.printf("set coeff to %s\n", Arrays.toString(this.coefficients));
   }

   /**
    * Given a string with coefficient values for exponents in ascending
    * order, parse the values and store them in our array of coefficient
    * values. 
    * Ignore trailing zero.
    *
    * TODO ignore multiple trailing zeros; set degree based on last non-zero
    * term ignoring those after
   // TODO not working correctly, also cumbersome
    *
    * @param coefficients String with coefficient values, separated by spaces
    * @return Array of coefficient values
    */
   private static double[] parseCoeff(String input) {
      String[] tokens = input.trim().split("\\s+");
      double[] coefficients = new double[tokens.length];

      for (int i = 0; i < tokens.length; ++i) {
         coefficients[i] = Double.parseDouble(tokens[i]);
      }

      System.out.printf("Parsed coefficients to array %s\n", Arrays.toString(coefficients));

      return coefficients;
   }

   private static int lastNonZero(double[] nums) {
      boolean isZero = true;
      int last = nums.length - 1;
      for (int i = last; i >= 0 && isZero; --i) {
         isZero = nums[i] == 0; 
         if (!isZero) { // TODO is this right?
            last = i;
         }
      }
      System.out.printf("Last non zero at index %d\n", last);
      return last;
   }

   private static double[] stripTrailingZeros(double[] nums) {
      int trimmedLength = Polynomial.lastNonZero(nums) + 1;
      double[] trimmed = new double[trimmedLength];
      for (int i = 0; i < trimmedLength; ++i) {
         trimmed[i] = nums[i];
      }
      System.out.printf("Trimmed array to length %d\n", trimmedLength);

      return trimmed;
   }

   /**
    * Set the coefficient array to a new array of values derived from the
    * given string.
    *
    * @param coefficientStr A string with space-separated values for the
    *           coefficients for exponents in ascending order
    */
   public void setCoeff(String coefficientStr) {
      this.setCoeff(Polynomial.parseCoeff(coefficientStr));
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

      return new Polynomial(coefficients);
   }

   private String coeffToString(int exponent) {
      System.out.printf("--- coeffToString (%d)\n", exponent);
      String output = new String();
      double coeff = this.getCoeff(exponent);

      if (coeff != 0) {
         String digitFormat = coeff == Math.floor(coeff) ? "%.0f" : "%.2f";
         String digits = String.format(digitFormat, Math.abs(coeff));

         String variable = exponent > 0 ? "x" : "";
         String power = exponent > 1 ? String.format("^%d", exponent) : "";

         return digits + variable + power;
      }
      return output;
   }

   /**
    * Return string representation of the polynomial.
    *
    * @return String
    */
   public String toString() {
      System.out.printf("--- toString: \n");
      StringBuilder output = new StringBuilder("f(x) = ");

      int degree = this.getDegree();

      if (degree == 0) {
         output.append("0");
      } else {
         int start;
         for (start = 0; 
               start < degree + 1 && this.getCoeff(start) == 0; 
               ++start) {
            // Just find first non-zero term
         }

         for (int i = start; i < degree + 1; ++i) {
            double thisCoeff = this.getCoeff(i);
            String operator = "";
            // TODO not working
            if (i == start) {
               operator = thisCoeff < 0 ? "-" : "";
            } else {
               operator = thisCoeff < 0 ? " - " : " + ";
            }
            output.append(operator + this.coeffToString(i));
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

      for (int i = 0; i < this.getDegree() + 1; ++i) {
         result += this.getCoeff(i) * Math.pow(x, i);
      }

      return result;
   }

   // TODO not working
   /**
    * Return a new Polynomial that is a derivative of this polynomial.
    * This class only allows non-negative integer exponents so we don't need
    * to deal with the derivative of 1/x.
    *
    * @return New Polynomial
    */
   public Polynomial derivative() {
      double[] derivCoefficients = new double[this.getDegree()];

      for (int i = 0; i < this.getDegree(); ++i) {
         derivCoefficients[i] = this.getCoeff(i + 1) * i;
         System.out.printf("deriv: setting new coeff to %.2f at index %d\n", derivCoefficients[i], i);
      }
      System.out.printf("deriv: create new polynomial with coefficients %s\n", Arrays.toString(derivCoefficients));
      return new Polynomial(derivCoefficients);
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
      double root = 0;
      // TODO START
      // throw new IllegalStateException("Did not converge within 1000
      // iterations");
      return root;
   }

}
