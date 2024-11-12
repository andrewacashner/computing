/* Circle
 * CSC101, Lab 2
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/12
 */

class Circle {

   public static void main(String[] args) {
      final double RADIUS = 99.9;

      double area = Math.PI * Math.pow(RADIUS, 2);
      double circumference = 2 * Math.PI * RADIUS;

      System.out.printf("For a circle with radius %.3f:\n", RADIUS);
      System.out.printf("%15s = %10.3f\n", "Area", area);
      System.out.printf("%15s = %10.3f\n", "Circumference", circumference);
   }
}
