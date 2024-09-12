/* Investment
 * CSC101, Lab 2
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/12
 */

class Investment {
    public static void main(String[] args) {
        final double PRINCIPAL_USD = 500.00;
        final double RATE_USD_ANNUAL_PERCENT = 3;
        final int YEARS_INVESTED = 10;

        double balance = PRINCIPAL_USD * 
            Math.pow((1 + RATE_USD_ANNUAL_PERCENT / 100), YEARS_INVESTED);

        System.out.printf(
                "$%.2f invested for %d years at %.2f%% yields $%.2f\n", 
                PRINCIPAL_USD, YEARS_INVESTED, RATE_USD_ANNUAL_PERCENT, 
                balance);
    }
}
