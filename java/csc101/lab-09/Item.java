/**
 * An item in a store's inventory.
 *
 * @author Andrew Cashner
 * @version 2024/11/01 (CSC101, Lab 9)
 */
public class Item {
   /** Description of the item */
   private String description;

   /** How many are in stock */
   private int quantity;

   /** Price in USD */
   private double price;

   public Item(String description, int quantity, double price) {
      this.description = description;
      this.quantity = quantity;
      this.price = price;
   }

   /** If quantity is omitted it is set to zero */
   public Item(String description, double price) {
      this(description, 0, price);
   }

   public String getDescription() {
      return this.description;
   }

   public int getQuantity() {
      return this.quantity;
   }

   public double getPrice() {
      return this.price;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setQuantity(int quantity) {
      this.quantity = quantity;
   }

   public void setPrice(double price) {
      this.price = price;
   }
}
