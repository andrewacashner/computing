import java.util.Scanner;

/** 
 * This class calculates the monthly payment required to pay back a loan.
 * We first query the user for initial loan value, annual interest rate, and
 * total number of years to pay off the loan.
 *
 * The monthly payment m needed given the principal p borrowed is 
 * <pre>m = p r (1 + r)^n / ((1 + r)^n - 1)</pre>
 * where r is a decimal interest rate per month (0.75 for 7.5%) 
 * and n is the total number of monthly payments.
 *
 * Loop the main function until user quits.
 * 
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/09  (CSC 101, Project 3)
 */
public class Payment {
    /** 
     * Query the user for needed loan information, then calculate and report
     * the monthly payment for this loan. Loop until quit.
     *
     * @param args (Unused) command-line arguments
     */
    public static void main(String[] args) {
        // Greeting
        System.out.print("LOAN PAYMENT CALCULATOR\n\n");

        Scanner kbScan = new Scanner(System.in);
        String input = new String("y");

        while (input.equals("y")) {
            // Get input
            System.out.print("Enter the initial loan amount (USD): $");
            double principalUSD = kbScan.nextDouble();

            System.out.print("Enter the annual interest rate (percent): %");
            double interestAnnualPercent = kbScan.nextDouble();

            System.out.print("Enter the total number of years of the loan: ");
            int periodYears = kbScan.nextInt();

            // Calculation
            double payment = monthlyPayment(principalUSD, 
                    interestAnnualPercent, periodYears);

            // Output
            System.out.printf("\nTo pay off a loan of $%,.2f at %.2f%% interest over %d years,\nyou will need to make a monthly payment of $%.2f.\n",
                    principalUSD, interestAnnualPercent, periodYears,
                    payment);

            System.out.print("\nCalculate another value? "
                    + "Enter y (or press any other key to quit): ");
            input = kbScan.next();
        } 

        // Closing
        System.out.print("\nThank you for using the Loan Payment Calculator.\n");
    }

    /** Calculate the monthly payment of a loan.
     *
     * @param principalUSD Starting amount of the loan, in US dollars
     * @param interestAnnualPercent Annual percentage rate (7 for 7%, not
     * 0.07)
     * @param periodYears Total years to pay off the loan
     *
     * @return Amount to be paid each month (in US dollars)
     */
    public static double monthlyPayment(double principalUSD,
            double interestAnnualPercent,
            int periodYears) {

        double rateMonthly = interestAnnualPercent / 1200;
        int totalPayments = periodYears * 12;
      
        // f = (1 + r)^n
        double factor = Math.pow((1 + rateMonthly), totalPayments);

        // m = p r f / (f - 1)
        double monthlyPayment = 
            principalUSD * rateMonthly * factor / (factor - 1);

        return monthlyPayment;
    }
}
