/* Wages
 * CSC101, Lab 2
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/12
 */

class Wages {
   public static void main(String[] args) {
      final int HOURS_WORKED = 47;

      final double HOURLY_WAGE_USD = 16.45;
      final int FULL_TIME_HOURS = 40;
      final double HOURLY_WAGE_OVERTIME_USD = 1.5 * HOURLY_WAGE_USD;

      // Assuming that 40 hours have been worked
      double base_earnings = HOURLY_WAGE_USD * FULL_TIME_HOURS;

      double overtime_earnings =
         HOURLY_WAGE_OVERTIME_USD * (HOURS_WORKED - FULL_TIME_HOURS);

      double total_earnings = base_earnings + overtime_earnings;

      System.out.printf("Wages being sent out = $%.2f\n", total_earnings);
   }
}
