/* # Discount: Coffee Sales and Shipping
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC101, Project 2
 * | October 2024
 *
 * ## Description
 *
 * The user says how many bags of coffee they want to buy; the program
 * calculates the total price including shipping, and reports how many boxes
 * the shipment will require. 
 *
 * The boxes come in three sizes, and medium and large bags must be full to
 * ship:
 *
 * ------------------------------------------------------------------------
 * Size     Capacity (in bags)   OK to Ship Partial Box?  Cost (per box)
 * -------  -------------------  ------------------------ -----------------
 * Large    20                   No                       $1.80
 * Medium   10                   No                       $1.00
 * Small     5                   Yes                      $0.60
 * ------------------------------------------------------------------------
 *
 * The price per bag is $5.50 each. The coffee price is discounted for 
 * larger quantities of coffee, according to the following schedule:
 *
 * -------------------
 *   Bags    Discount
 * -------   ---------
 *   0--24   None
 *  25--49     5%
 *  50--99    10%
 * 100--149   15%
 * 150--199   20%
 * 200--299   25%
 * 300+       30%
 * -------------------
 */

import java.util.Scanner;

class Discount {

   // Create table row with three columns of width 30, 10, and 10
   // Format currency consistently
   static String receiptRow(String description, String details, double price) {
       return String.format("%30s %10s %10s\n", 
               description, 
               details, 
               String.format("$%.2f", price));
   }

   // Row with only label (e.g., for section heading)
   static String receiptRow(String description) {
       return String.format("%30s\n", description);
   }

   // Row showing number of boxes ordered and cost of boxes
   static String receiptRowBoxes(String description, int amount, double price) {
       return Discount.receiptRow("", 
               String.format("%3d %6s", amount, description),
               price);
   }


   public static void main(String[] args) {
      // Constants
      final double PRICE_PER_BAG_USD = 5.50;
      
      final int BAGS_PER_BOX_LARGE  = 20;
      final int BAGS_PER_BOX_MEDIUM = 10;
      final int BAGS_PER_BOX_SMALL  = 5;

      final double BOX_PRICE_LARGE_USD  = 1.80;
      final double BOX_PRICE_MEDIUM_USD = 1.00;
      final double BOX_PRICE_SMALL_USD  = 0.60;

      // Welcome, Get input
      System.out.print(" ~~~ Welcome to the Discount Coffee Store ~~~\n\n");

      Scanner kbScan = new Scanner(System.in);
      System.out.print("Enter the number of bags of coffee to order: ");
      int bagsOrdered = kbScan.nextInt();

      // Calculate coffee price: Determine discount (if any) and substract
      // from the total at the default rate
      int discountPercent;

      if (bagsOrdered < 25) {
         discountPercent = 0;
      } else if (bagsOrdered < 50) {
         discountPercent = 5;
      } else if (bagsOrdered < 100) {
         discountPercent = 10;
      } else if (bagsOrdered < 150) {
         discountPercent = 15;
      } else if (bagsOrdered < 200) {
         discountPercent = 20;
      } else if (bagsOrdered < 300) {
         discountPercent = 25;
      } else {
         discountPercent = 30;
      }

      double coffeePriceFullRateUSD = bagsOrdered * PRICE_PER_BAG_USD;
      double discountAmountUSD = 
         coffeePriceFullRateUSD * -(double)discountPercent / 100;
      double coffeePriceUSD = coffeePriceFullRateUSD + discountAmountUSD;

      // Calculate boxes needed for shipping
      int largeBoxes = 0;
      int mediumBoxes = 0;
      int smallBoxes = 0;

      // After each category is dealt with, check the next
      int bagsLeft = bagsOrdered;
      if (bagsLeft >= BAGS_PER_BOX_LARGE) {
         largeBoxes = bagsLeft / BAGS_PER_BOX_LARGE;
         bagsLeft %= BAGS_PER_BOX_LARGE;
      } 
      if (bagsLeft >= BAGS_PER_BOX_MEDIUM) {
         mediumBoxes = bagsLeft / BAGS_PER_BOX_MEDIUM;
         bagsLeft %= BAGS_PER_BOX_MEDIUM;
      }
      if (bagsLeft >= BAGS_PER_BOX_SMALL) {
         smallBoxes = bagsLeft / BAGS_PER_BOX_SMALL;
         bagsLeft %= BAGS_PER_BOX_SMALL;
      }
      if (bagsLeft > 0) { // Small boxes don't have to be full
         ++smallBoxes;
      }

      // Calculate shipping price and total
      double largeBoxPrice  = largeBoxes  * BOX_PRICE_LARGE_USD;
      double mediumBoxPrice = mediumBoxes * BOX_PRICE_MEDIUM_USD;
      double smallBoxPrice  = smallBoxes  * BOX_PRICE_SMALL_USD;

      double shippingPriceUSD = 
         largeBoxPrice + mediumBoxPrice + smallBoxPrice;
    
      double totalPriceUSD = coffeePriceUSD + shippingPriceUSD;


      // Report order details in a receipt table: Create as a single string
      String receipt = new String("\n");

      receipt += Discount.receiptRow("Number of Bags Ordered:", 
                  String.format("%d", bagsOrdered), 
                  coffeePriceFullRateUSD);

      if (discountPercent > 0) {
         receipt += Discount.receiptRow("Discount:", 
                     String.format("%d%%", discountPercent),
                     discountAmountUSD);
      }

      // Only show boxes that were used
      receipt += Discount.receiptRow("Boxes Used:");

      if (largeBoxes > 0) {
         receipt += Discount.receiptRowBoxes("Large", largeBoxes, largeBoxPrice);
      }

      if (mediumBoxes > 0) {
          receipt += Discount.receiptRowBoxes("Medium", mediumBoxes, mediumBoxPrice);
      }

      if (smallBoxes > 0) {
          receipt += Discount.receiptRowBoxes("Small", smallBoxes, smallBoxPrice);
      }

      receipt += Discount.receiptRow("Total Price:", "", totalPriceUSD);

      // Print results
      System.out.print(receipt);
      System.out.print("\nThank you for shopping at the Discount Coffee Store!\n");
   }
}