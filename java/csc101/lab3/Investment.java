/* Investment Calculator
 *
 * Given the principal, rate of return (percentage) and number of years of
 * investment, output the final balance of an investment.
 *
 * CSC101, Lab 3
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/19
 */

import java.util.Scanner;

class Investment {
   public static void main(String[] args) {
      System.out.println("INVESTMENT CALCULATOR");

      // Get user input
      Scanner kbScan = new Scanner(System.in);

      System.out.print("Enter the initial principal you are investing, in US dollars: $");
      double principalUSD = kbScan.nextDouble();

      System.out.print("Enter the annual percentage rate of return (Enter 3 for 3%, not .03): %");
      double rateAnnualPercentUSD = kbScan.nextDouble();

      System.out.print("Please enter the number of years for this investment: ");
      int yearsInvested = kbScan.nextInt();

      /* Calculate investment
       *    A = P(1 + r/100)^t
       *    where A = balance, r = rate, P = initial value, t = years invested
       */
      double balance = principalUSD * 
         Math.pow((1 + rateAnnualPercentUSD / 100), yearsInvested);

      // Show output
      System.out.printf(
            "\n$%.2f invested for %d years at %.2f%% yields $%.2f\n", 
            principalUSD, yearsInvested, rateAnnualPercentUSD, 
            balance);
   }
}
