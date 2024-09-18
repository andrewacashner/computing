/* Change
 * CSC101 Project 1
 * Andrew Cashner, 2024/09/17
 */

import java.util.Scanner;

class Change {

   static final int DOLLAR  = 100;
   static final int QUARTER =  25;
   static final int DIME    =  10;
   static final int NICKEL  =   5;
   static final int PENNY   =   1;

   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);

      System.out.println("CHANGE CALCULATOR");
      System.out.print("Enter price of product: $");
      double price = kbScan.nextDouble();

      System.out.print("Enter amount tendered:  $");
      double payment = kbScan.nextDouble();

      // Assuming payment is more than price
      double changeTotal = payment - price;

      // Round to avoid inaccuracy in conversion to int
      int changeLeft = (int)Math.round(changeTotal * 100);

      int changeDollars = changeLeft / DOLLAR;
      changeLeft %= DOLLAR;

      int changeQuarters = changeLeft / QUARTER;
      changeLeft %= QUARTER;

      int changeDimes = changeLeft / DIME;
      changeLeft %= DIME;

      int changeNickels = changeLeft / NICKEL;
      changeLeft %= NICKEL;

      int changePennies = changeLeft / PENNY;

      System.out.printf("Total change: $%.2f\n\n", changeTotal);
      System.out.printf("  %-10s%5d\n", "Dollars", changeDollars);
      System.out.printf("  %-10s%5d\n", "Quarters", changeQuarters);
      System.out.printf("  %-10s%5d\n", "Dimes", changeDimes);
      System.out.printf("  %-10s%5d\n", "Nickels", changeNickels);
      System.out.printf("  %-10s%5d\n", "Pennies", changePennies);
   }
}
