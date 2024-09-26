/* Wages2
 *
 * Given a number of hours worked, an hourly wage, and the number of hours
 * counted as full time, calculate and output the wages earned.
 * Overtime is paid a 1.5 times the base wage.
 *
 * Negative input results in an error message.
 *
 * CSC101, Lab 4
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/26
 */

import java.util.Scanner;

class Wage2 {
   public static void main(String[] args) {
      // Welcome, get input
      System.out.println("WAGE CALCULATOR");

      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter number of hours worked: ");
      double hoursWorked = kbScan.nextDouble();

      if (hoursWorked < 0) {
         System.err.println("Invalid calculation (hours worked cannot be negative)");
         /* Would be better just to exit here */
      } else {
         System.out.print("Enter hourly wage: $");
         double baseWagePerHourUSD = kbScan.nextDouble();

         System.out.print("Enter number of hours counted as full time: ");
         double fullTimeHours = kbScan.nextDouble();

         final double overtimeWagePerHourUSD = 1.5 * baseWagePerHourUSD;

         // Calculate, including overtime if needed
         double earnings;

         if (hoursWorked <= fullTimeHours) {
            earnings = baseWagePerHourUSD * hoursWorked;
         } else {
            double baseEarnings = baseWagePerHourUSD * fullTimeHours;
            double overtimeEarnings = 
               overtimeWagePerHourUSD * (hoursWorked - fullTimeHours);

            earnings = baseEarnings + overtimeEarnings;
         }

         // Report earnings
         System.out.printf("Earnings: $%.2f\n", earnings);
      }
   }
}
