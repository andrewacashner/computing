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
 * @version 2024/11/20 CSC 101, Project 5)
 */
public class Polynomial {

   /** Highest exponent of the expression */
   private int degree;

   /** Array of coefficients in ascending order of exponents */
   private double[] coefficients;

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
      setCoeff(new double[1]);
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
      double[] result;

      if (nums.length == 0 || nums[nums.length - 1] != 0) {
         result = nums;
      } else {
         int trimmedLength = lastNonZero(nums) + 1;
         double[] trimmed = new double[trimmedLength];
         for (int i = 0; i < trimmedLength; ++i) {
            trimmed[i] = nums[i];
         }
         result = trimmed;
      }
      return result;
   }

   // ADDING POLYNOMIALS
   /**
    * Return a new polynomial that is the sum of this one and another.
    *
    * @param other Polynomial to add
    * @return New Polynomial
    */
   public Polynomial add(Polynomial other) {
      // The sum array will match the degree of the highest-degree summand.
      int thisDegree = this.getDegree();
      int otherDegree = other.getDegree();
      
      int length;
      if (thisDegree > otherDegree) {
         length = thisDegree + 1;
      } else {
         length = otherDegree + 1;
      }
      double[] coefficients = new double[length];

      for (int i = 0; i < length; ++i) {
         coefficients[i] = this.getCoeff(i) + other.getCoeff(i);
      }

      return new Polynomial(coefficients);
   }

   // STRING CONVERSION
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
    * @param isFirst True if this is the first coefficient to be printed
    * @return String representation of coefficient and variable
    */
   private String coeffToString(int exponent, boolean isFirst) {

      StringBuilder output = new StringBuilder();
      double coeff = this.getCoeff(exponent);

      if (coeff != 0) {
         // No operator for first term, just negative sign if needed
         if (isFirst) {
            if (coeff < 0) {
               output.append("-");
            }
         } else if (coeff > 0) {
            output.append(" + ");
         } else {
            output.append(" - ");
         }

         // No digit shown if coefficient is 1 and exponent is positive
         double absCoeff = Math.abs(coeff);
         if (!(absCoeff == 1 && exponent > 0)) {
         
            // No decimal shown if coefficient is an integer
            if (Math.floor(absCoeff) == absCoeff) {
               output.append(String.format("%.0f", absCoeff));
            } else {
               output.append(String.format("%.2f", absCoeff));
            }
         }

         if (exponent > 0) {
            // No variable shown if exponent is 0
            output.append("x");

            // No exponent shown if exponent is 1
            if (exponent > 1) {
               output.append(String.format("^%d", exponent));
            }
         }
      }

      return output.toString();
   }

   /**
    * Return string representation of the polynomial. A polynomial with
    * coefficients <code>[-1, 1, -2, 0, 3]</code> and given functionName
    * "f" and variable "x" will return
    * <code>"f(x) = -1 + x - 2x^2 + 3x^4"</code>.
    * 
    * @param functionName String name of function
    * @return String representation
    */
   public String toString() {

      StringBuilder output = new StringBuilder("f(x) = ");

      int degree = this.getDegree();

      if (degree == 0) {
         output.append("0");
      } else {
         int start = this.firstNonZero(this.coefficients);
         for (int i = start; i < this.coefficients.length; ++i) {

            if (this.getCoeff(i) != 0) {
               boolean isFirst = i == start;
               output.append(this.coeffToString(i, isFirst));
            }
         }
      }
      return output.toString();
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
    * iterating until the desired precision or throwing an error if too many
    * iterations.
    *
    * @param guess Double initial guess value
    * @return Double root
    * @throws IllegalStateException if the method does not converge within
    *           maximum iterations
    */
   public double findRoot(double guess) throws IllegalStateException {

      double root = guess;

      final int MAX_ITERATIONS = 1000;
      final double PRECISION = 0.00001;

      double prev = 0;
      int i = 0;
      do {
         ++i;
         if (i > MAX_ITERATIONS) {
            throw new IllegalStateException(
                  "Did not converge within 1000 iterations");
         }

         double thisValue = this.evaluate(root);
         double thisDerivValue = this.derivative().evaluate(root);
         prev = root;
         root -= thisValue / thisDerivValue;

      } while (Math.abs(root - prev) > PRECISION);

      return root;
   }
}
