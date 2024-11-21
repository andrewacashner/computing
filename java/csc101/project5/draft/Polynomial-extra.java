/**
 * This class models a polynomial function.
 * It records the degree (highest exponent) of the function and the
 * coefficients for each term in ascending order.
 * Methods allow for adding, evaluating, differentiating, and finding the
 * root of the polynomial.
 *
 * Note: We are assuming that coefficients are non-negative integers
 * ascending from zero.  
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/11/12 (CSC 101, Project 5)
 */
public class Polynomial {

   /** 
    * Main method for testing 
    *
    * @param args Command-line arguments: Requires a string of numbers
    *   representing coefficients for exponents in ascending order
    * */
   public static void main(String[] args) {
      if (args.length < 1) {
         System.err.println("Usage: java Polynomial \"c0 c1 c2 c3 ...\"");
         return;
      }

      String input = args[0];
      Polynomial fn;
      
      try {
         fn = new Polynomial(input);
         System.out.printf("Created polynomial %s\n", fn);

      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;
      }

      for (double x = 0; x < 5.0; ++x) {
         System.out.printf("Evaluate f(%.0f) = %.4f\n", x, fn.evaluate(x));
      }

      try {
         Polynomial empty = new Polynomial();
         System.out.println(empty.toString("empty", "x"));

         Polynomial a = new Polynomial("1 2 3");
         Polynomial b = new Polynomial("1 1 1 1");
         Polynomial c = a.add(b);
         System.out.printf("Add:\n     %s\n   + %s\n   = %s\n", 
            a.toString("a", "x"),
            b.toString("b", "x"),
            c.toString("c", "x"));
      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;
      }

      Polynomial d = fn.derivative();
      System.out.printf("Derivative of user fn: %s\n", 
            d.toString("f'", "x"));

      try {
         Polynomial e = new Polynomial("-4.5 1.5 1");
         double root = e.findRoot(5);
         System.out.printf("Root of polynomial {%s} ≈ %.6f\n", e, root);

      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;

      } catch (IllegalStateException e) {
         System.err.println(e.getMessage());
         return;
      }

   }

   // INSTANCE MEMBERS
   /** Highest exponent of the expression */
   private int degree;

   /** Array of coefficients in ascending order of exponents */
   private double[] coefficients;

   // STATIC MEMBERS
   /** Maximum number of iterations of Newton-Raphson method */
   private static final int MAX_ROOT_ITERATIONS = 1000;

   /** 
    * Acceptable precision (difference between successive iterations of
    * Newton-Raphson method) for finding the root
    */
   private static final double ROOT_PRECISION = 0.00001;

   // CONSTRUCTORS
   /** 
    * Create a polynomial from an array of coefficients. The degree is one
    * less than the length of the array. By calling setCoeff, we also trim
    * the coefficient array to remove trailing zeros and update degree to
    * match the trimmed array.
    *
    * @param coefficients Array of doubles for coefficients to exponents in
    *           ascending order
    */
   private Polynomial(double[] rawCoefficients) {
      setCoeff(rawCoefficients); 
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
    * @throws NumberFormatException if the input could not be parsed
    *       correctly
    * @see #setCoeff(String)
    */
   public Polynomial(String coefficientStr) throws NumberFormatException {
      setCoeff(coefficientStr);
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

      if (exponent <= this.getDegree()) {
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
    * Set the degree to one less than the length of the stored coefficient
    * array.
    */
   private void setDegree() {
      this.degree = this.coefficients.length - 1;
   }

   // CREATING THE LIST OF COEFFICIENTS
   /**
    * Assign a new array of coefficients with trailing zeros stripped and
    * update degree to match the new array.
    *
    * @param coefficients Double array of coefficient values
    */
   private void setCoeff(double[] coefficients) {
      this.coefficients = stripTrailingZeros(coefficients);
      this.setDegree();
   }

   /**
    * Set the coefficient array to a new array of values derived from the
    * given string.
    *
    * @param coefficientStr A string with space-separated values for the
    *           coefficients for exponents in ascending order
    * @throws NumberFormatException if there was a problem reading the input
    */
   public void setCoeff(String coefficientStr) 
        throws NumberFormatException {

      double[] coefficients = parseCoeff(coefficientStr);
      this.setCoeff(coefficients);
   }

   /**
    * Given a string with coefficient values for exponents in ascending
    * order, parse the values and store them in our array of coefficient
    * values. 
    *
    * @param coefficients String with coefficient values, separated by spaces
    * @return Array of coefficient values
    * @throws NumberFormatException if there was a problem reading the input
    */
   private static double[] parseCoeff(String input) 
         throws NumberFormatException {

      String[] tokens = input.trim().split("\\s+");
      double[] coefficients = new double[tokens.length];

      for (int i = 0; i < tokens.length; ++i) {
         try {
            coefficients[i] = Double.parseDouble(tokens[i]);
         }
         catch (NumberFormatException e) {
            throw new NumberFormatException(
                  String.format("Invalid input for coefficient \"%s\"", tokens[i]));
         }
      }

      return coefficients;
   }

   /**
    * Return the index of the first nonzero term of an array of numbers.
    *
    * @param nums Double array 
    * @return Index of first nonzero term
    */
   private int firstNonZero(double[] nums) {
      int start = 0;
      int i;
      for (i = 0; i < nums.length && nums[i] == 0; ++i) {
         // Just find first nonzero term
      }
      start = i;
      return start;
   }

   /**
    * Return the index of the last nonzero term of an array of numbers, with
    * a minimum of 0.
    *
    * @param nums Double array
    * @return Index of last nonzero term
    */
   private static int lastNonZero(double[] nums) {
      int last = nums.length - 1;
      int i;
      for (i = last; i > 0 && nums[i] == 0; --i) {
         // Just find last nonzero index
      }
      last = i;

      return last;
   }

   /**
    * Return a new array of numbers with a run of one or more zeros at the
    * end removed.
    *
    * @param nums Double array
    * @return New shorter array without the trailing zeros
    */
   private static double[] stripTrailingZeros(double[] nums) {
      if (nums.length == 0 || nums[nums.length - 1] != 0) {
         return nums;
      } else {
         int trimmedLength = lastNonZero(nums) + 1;
         double[] trimmed = new double[trimmedLength];
         for (int i = 0; i < trimmedLength; ++i) {
            trimmed[i] = nums[i];
         }
         return trimmed;
      }
   }

   // ADDING POLYNOMIALS
   /**
    * Return the higher of two polynomials, that is, the one with a higher
    * degree.
    *
    * @param a First polynomial
    * @param b Second polynomial
    * @return Reference to the polynomial with higher degree
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
      int length = higher(this, other).getDegree() + 1;
      double[] coefficients = new double[length];

      for (int i = 0; i < length; ++i) {
         coefficients[i] = this.getCoeff(i) + other.getCoeff(i);
      }

      return new Polynomial(coefficients);
   }

   // STRING CONVERSION
   /** 
    * A term only includes the variable if exponent is greater than zero.
    *
    * @param exponent Integer exponent
    * @param varName String, name of variable
    * @return String for variable
    */
   private String varToString(int exponent, String varName) {
      return exponent > 0 ? varName : "";
   }

   /**
    * Only show exponents greater than 1.
    *
    * @param exponent Integer exponent
    * @return String for exponent
    */
   private String expToString(int exponent) {
      return exponent > 1 ? String.format("^%d", exponent) : "";
   }

   /**
    * Convert a single coefficient value to a string, using this format:
    * <ol>
    * <li>Initial values appear alone; negative initial values have preceding
    * negative sign.</li>
    * <li>Successive values are separated by "+" or "-" dependent on the
    * value of the next value.</li>
    * <li>Coefficients of 1 for exponents more than zero are omitted.</li>
    * <li>Variables with exponents of zero are omitted.</li>
    * </ol>
    *
    * @param exponent Integer value of this coefficient's exponent
    * @param varName String, name of variable used in this function
    * @return String representation of coefficient and variable
    */
   private String coeffToString(int exponent, String varName) {

      String output = new String();
      double coeff = this.getCoeff(exponent);
      double absCoeff = Math.abs(coeff);

      if (absCoeff > 0) {

         String digits;
         if (absCoeff == 1 && exponent > 0) {
            digits = "";
         } else if (absCoeff == Math.floor(absCoeff)) {
            digits = String.format("%.0f", absCoeff);
         } else {
            digits = String.format("%.2f", absCoeff);
         }

         return digits + 
            varToString(exponent, varName) + 
            expToString(exponent);
      }
      return output;
   }

   /**
    * Return a list of string operator symbols to prefix to each element in a
    * numeric array. (Equivalent to mapping an operatorString function over
    * the array.)
    * <p>
    * We treat the first item differently because it doesn't have
    * the preceding <code>" + "</code> and if negative, the sign is
    * immediately next to the coefficient.
    * </p>
    *
    * @param nums Double array
    * @return String array of operator symbols
    */
   private String[] operators(double[] nums) {
      String [] operatorList = new String[nums.length];

      if (nums.length > 0) {
         int start = this.firstNonZero(nums);
         operatorList[start] = nums[start] > 0 ? "" : "-";

         for (int i = start + 1; i < nums.length; ++i) {
            operatorList[i] = nums[i] > 0 ? " + " : " - ";
         }
      }

      return operatorList;
   }

   /**
    * Return string representation of the polynomial. A polynomial with
    * coefficients <code>[-1, 1, -2, 0, 3]</code> and given functionName
    * "f" and variable "x" will return
    * <code>"f(x) = -1 + x - 2x^2 + 3x^4"</code>.
    * 
    * @param functionName String name of function
    * @param varName String name of variable
    * @return String representation
    */
   public String toString(String functionName, String varName) {

      StringBuilder output = new StringBuilder(
            String.format("%s(%s) = ", functionName, varName));

      int degree = this.getDegree();

      if (degree == 0) {
         output.append("0");
      } else {
         String[] operatorList = this.operators(this.coefficients);

         int start = this.firstNonZero(this.coefficients);
         for (int i = start; i < this.coefficients.length; ++i) {

            if (this.getCoeff(i) != 0) {
               output.append(operatorList[i] + 
                     this.coeffToString(i, varName));
            }
         }
      }
      return output.toString();
   }

   /**
    * Default string representation uses function name f(x).
    *
    * @return String representation
    */
   public String toString() {
      return this.toString("f", "x");
   }

   // EVALUATING f(x) FOR A PARTICULAR x
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

   // FINDING THE DERIVATIVE
   /**
    * Return a new Polynomial that is a derivative of this polynomial.
    * This class only allows non-negative integer exponents so we don't need
    * to deal with the derivative of 1/x.
    *
    * @return New Polynomial
    */
   public Polynomial derivative() {
      // The x^0 term disappears so the derivative has one less coefficient
      double[] derivCoefficients = new double[this.getDegree()]; 

      for (int i = 0; i < this.getDegree(); ++i) {
         derivCoefficients[i] = this.getCoeff(i + 1) * (i + 1);
      }

      return new Polynomial(derivCoefficients);
   }

   // FINDING ROOTS
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
   public double findRoot(double guess) throws IllegalStateException {
      return this.findRoot(guess, 0, MAX_ROOT_ITERATIONS, ROOT_PRECISION);
   }

   /**
    * Calculate the root using the Newton-Raphson method with the given
    * initial guess, iterating until the desired accuracy.
    *
    * @param guess Double initial guess value
    * @param iteration How many iterations have we done?
    * @param maxIterations Maximum number of iterations before giving up
    * @param precision Desired precision (difference between one guess and
    *           the next)
    *
    * @return Double root (next guess)
    *
    * @throws IllegalStateException if the method does not converge within
    *           max number of iterations
    */
   private double findRoot(double guess, int iteration, int maxIterations, 
         double precision) throws IllegalStateException {

      if (iteration > maxIterations) {
         throw new IllegalStateException(
               "Did not converge within 1000 iterations");
      }

      double root = 
         guess - this.evaluate(guess) / this.derivative().evaluate(guess);

      if (Math.abs(guess - root) < precision) {
         return root;
      } else {
         return this.findRoot(root, iteration + 1, maxIterations, precision);
      }
   }
}
