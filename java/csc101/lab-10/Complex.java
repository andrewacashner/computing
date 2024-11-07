/** 
 * Class for modelling complex numbers
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/11/07 (CSC 101, Lab 10)
 */
public class Complex {

   /** Maximum size (offset from origin) */
   private static double LIMIT = 10.0;

   /** The real portion of the imaginary number */
   private double real;

   /** The imaginary portion (factor of i) */
   private double imaginary;

   /** 
    * Create a complex from two doubles for real and imaginary portions 
    * @param real Double
    * @param imaginary Double
    */
   public Complex(double real, double imaginary) {
      this.real = real;
      this.imaginary = imaginary;
   }

   /**
    * Create a complex number equal to zero
    */
   public Complex() {
      this(0, 0);
   }

   /** 
    * Create a complex number from only the real portion (imaginary portion is
    * zero
    *
    * @param real Double
    */
   public Complex(double real) {
      this(real, 0);
   }

   /**
    * Accessor for real porition
    *
    * @return Double
    */
   public double getReal() {
      return this.real;
   }

   /**
    * Accessor for imaginary portion
    *
    * @return Double
    */
   public double getImaginary() {
      return this.imaginary;
   }

   /**
    * Set the real value
    *
    * @param real Double
    */
   public void setReal(double real) {
      this.real = real;
   }

   /**
    * Set the imaginary value
    *
    * @param imaginary Double
    */
   public void setImaginary(double imaginary) {
      this.imaginary = imaginary;
   }

   /**
    * Return a string representation of the complex number in the format 
    * <em>a + bi</em> where <em>a</em> is the real portion and <em>b</em> the
    * imaginary factor
    *
    * @return String
    */
   public String toString() {
      return String.format("%.1f + %.1fi", this.real, this.imaginary);
   }

   /**
    * Is this Complex number equal to another? (Are both members equal to the
    * corresponding members of the other?)
    *
    * @param other Complex number to compare
    * @return True if both are equal
    */
   public boolean equals(Complex other) {
      return this.real == other.real
            && this.imaginary == other.imaginary;
   }

   /**
    * Return a new Complex number that is the sum of this one and another.
    * (Add the real members and the imaginary members.)
    *
    * @param other Complex number to add to this one
    * @return New Complex number
    */
   public Complex add(Complex other) {
      return new Complex(this.real + other.real, 
            this.imaginary + other.imaginary);
   }

   /**
    * Return a new Complex number that is the sum of this one and a given
    * double value (added to the real portion).
    *
    * @param value Double
    * @return New Complex number
    */
   public Complex add(double value) {
      return new Complex(this.real + value, this.imaginary);
   }

   /**
    * Static method to add two Complex numbers
    *
    * @param first Complex
    * @param second Complex
    * @return New Complex number (sum of first and second)
    */
   public static Complex add(Complex first, Complex second) {
      return first.add(second);
   }

   /**
    * Is this number bigger than the preset limit?
    * True if the offset <code>sqrt(real^2 = imaginary^2) &gt; limit</code>.
    *
    * @return True if bigger
    */
   public boolean isBig() {
      double distance = Math.sqrt(Math.pow(this.real, 2) 
            + Math.pow(this.imaginary, 2));

      return distance > this.LIMIT;
   }
}
