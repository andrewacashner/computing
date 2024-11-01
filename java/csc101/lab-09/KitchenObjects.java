/**
 * An inventory of kitchen items
 */
public class KitchenObjects {

   /** Create an inventory, manipulate the items, and print the contents. */
   public static void main(String[] args) {

      Item spoon = new Item("Spoon", 14, 1.25);
      Item fork = new Item("Fork", 18, 1.50);
      Item knife = new Item("Knife", 12, 2.35);
      Item dish = new Item("Dish", 5.95);
      Item bowl = new Item("Bowl", 4.99);

      Item[] inventory = { spoon, fork, knife, dish, bowl };

      fork.setPrice(1.75);
      dish.setQuantity(5);
      bowl.setQuantity(8);
      knife.setDescription("Butter Knife");

      for (Item thisItem: inventory) {
         System.out.printf("We have %d %s(s) at a price of $%.2f each\n",
               thisItem.getQuantity(),
               thisItem.getDescription(),
               thisItem.getPrice());
      }
   }
}
