/* Triangle */

import java.util.Scanner;

/** 
 * Calculate the area, hypotenuse length, and perimeter of a right triangle
 * given the other two sides.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/10 (CSC 101, Project 3)
 */
public class Triangle {
   /**
    * Receive two user inputs as doubles for the two smaller sides of a right
    * triangle.
    * Calculate the area, length of the hypotenuse, and perimeter and report
    * the results.
    * Loop until the user quits.
    *
    * @param args (Unused) command-line arguments
    */
   public static void main(String[] args) {
      System.out.print("TRIANGLE CALCULATOR\n");
      System.out.print(
            "Given the length of the two shorter sides of a triangle,\n"
            + "calculate area, hypotenuse, and perimeter.\n\n");

      Scanner kbScan = new Scanner(System.in);
      String input = new String("y");

      while (input.equals("y")) {
         // Get user input
         System.out.print("Enter the lengths of two sides of a triangle: ");
         double sideA = kbScan.nextDouble();
         double sideB = kbScan.nextDouble();
         kbScan.nextLine(); // Clear newline from buffer

         // Do calculations
         double area = triangleArea(sideA, sideB);
         double hypotenuse = hypotenuse(sideA, sideB);
         double perimeter = trianglePerimeter(sideA, sideB);

         // Report results as a table
         System.out.print(triangleTable(area, hypotenuse, perimeter));

         System.out.print("\nTri again? Press y (or any other key to quit): ");
         input = kbScan.nextLine();
      }

      System.out.print("\nThank you for using the Triangle Calculator.\n");
   }

   /**
    * Calculates the area of a right triangle.
    * For a triangle with the two shorter sides a and b, area = ab/2.
    *
    * @param sideA One leg
    * @param sideB The other leg
    *
    * @return The area
    */
   public static double triangleArea(double sideA, double sideB) {
      return sideA * sideB / 2;
   }

   /**
    * Calculates the length of the hypotenuse of a right triangle.
    * According to the Pythagorean theorem, for a right triangle with the two
    * shorter sides a and b, <code>h^2 = a^2 + b^2</code>, 
    * therefore <code>h = sqrt(a^2 + b^2)</code>.
    *
    * @param sideA One side
    * @param sideB The other side
    *
    * @return Length of hypotenuse
    */
   public static double hypotenuse(double sideA, double sideB) {
      return Math.sqrt(Math.pow(sideA, 2) + Math.pow(sideB, 2));
   }

   /**
    * Calculates the perimeter of a right triangle given the two shorter
    * sides.
    * The perimeter is the sum of the lengths of the sides.
    *
    * @param sideA One side
    * @param sideB The other side
    *
    * @return Perimeter length
    */
   public static double trianglePerimeter(double sideA, double sideB) {
      return sideA + sideB + hypotenuse(sideA, sideB);
   }

   /** 
    * Single row of the two-column triangle data table.
    *
    * @param label Description (left column)
    * @param value Numeric value (right column)
    
    * @return Row including newline
   
    * @see triangleTable
    */
   public static String tableRow(String label, double value) {
      return String.format("| %-15s | %15.4f |\n", label, value);
   }

   /**
    * Create a table showing the area, hypotenuse, and perimeter of a right
    * triangle.
    *
    * @param area Triangle area
    * @param hypotenuse Triangle hypotenuse
    * @param perimeter Triangle perimeter
    *
    * @return Table as a string
    */
   public static String triangleTable(double area, double hypotenuse, 
         double perimeter) {

      String tableRule = 
         new String("-------------------------------------\n");

      String table = tableRule + 
         tableRow("Area", area) +
         tableRow("Hypotenuse", hypotenuse) +
         tableRow("Perimeter", perimeter) +
         tableRule;

      return table;
   }


}
