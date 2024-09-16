/* Change
 * CSC101 Project 1
 * Andrew Cashner, 2024/09/16
 */

import java.util.Scanner;

class Change {

   static final int DOLLAR  = 100;
   static final int QUARTER =  25;
   static final int DIME    =  10;
   static final int NICKEL  =   5;
   static final int PENNY   =   1;

   static int change(int total, int value) {
      return total / value;
   }

   static int remainder(int total, int value, int quantity) {
      return total - (value * quantity);
   }

   static String changeRow(String label, int amount) {
      return String.format("  %-10s%5d\n", label, amount);
   }

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

      int changeDollars = change(changeLeft, DOLLAR);
      changeLeft = remainder(changeLeft, DOLLAR, changeDollars);

      int changeQuarters = change(changeLeft, QUARTER);
      changeLeft = remainder(changeLeft, QUARTER, changeQuarters);

      int changeDimes = change(changeLeft, DIME);
      changeLeft = remainder(changeLeft, DIME, changeDimes);

      int changeNickels = change(changeLeft, NICKEL);
      changeLeft = remainder(changeLeft, NICKEL, changeNickels);

      int changePennies = change(changeLeft, PENNY);

      String table = 
         changeRow("Dollars", changeDollars) +
         changeRow("Quarters", changeQuarters) + 
         changeRow("Dimes", changeDimes) + 
         changeRow("Nickels", changeNickels) +
         changeRow("Pennies", changePennies);

      System.out.printf("Total change: $%.2f\n\n", changeTotal);
      System.out.print(table);
   }
}
