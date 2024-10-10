/* Triangle */

import java.util.Scanner;

/** 
 * Calculate the area, hypotenuse length, and perimeter of a right triangle
 * given the other two sides.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/10 (CSC 101, Project 3)
 */
class Triangle {
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
      System.out.println("TRIANGLE CALCULATOR");
      System.out.println("Calculate area, hypotenuse, and perimeter given the length of the two shorter sides of a triangle.");

      Scanner kbScan = new Scanner(System.in);
      String input = new String("y");

      while (input.equals("y")) {
         // Get user input
         System.out.print("Enter the length of the two shorter sides of a triangle:");
         double sideA = kbScan.nextInt();
         double sideB = kbScan.nextInt();

         // Do calculations
         double area = triangleArea(sideA, sideB);
         double hypotenuse = hypotenuse(sideA, sideB);
         double perimeter = trianglePerimeter(sideA, sideB);

         // Report results as a table
         System.out.print(triangleInfoTable(area, hypotenuse, perimeter));

         System.out.print("Tri again? Press y (or any other key to quit): ");
      }

      System.out.println("Thank you for using the Triangle Calculator.");
   }

   public static double triangleArea(double sideA, sideB) {
      double area;
      return area;
   }

   public static double hypotenuse(double sideA, sideB) {
      double hypotenuse;
      return hypotenuse;
   }

   public static double trianglePerimter(double sideA, sideB) {
      double perimeter;
      return perimeter;
   }

   public static String triangleTable(double area, hypotenuse, perimeter) {
      String table = new String();
      return table;
   }


}
