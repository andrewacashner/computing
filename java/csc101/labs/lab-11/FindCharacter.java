import java.util.Scanner;

/**
 * Return the character at a given index from a given string.
 * Catch any exceptions for out-of-bounds indices.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/11/21 (CSC 101, Lab 11)
 */
public class FindCharacter {

   /**
    * Loop to ask for user input string and index; print the character at
    * that index unless it is out of range.
    *
    * @param args Unused command-line arguments
    */
   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);
      System.out.println("FIND CHARACTER: Find a character at a given index");

      String inputString = null;
      while (inputString == null || !inputString.isEmpty()) {
         System.out.println("Enter a string to search (Leave blank and press enter to quit): ");
         inputString = kbScan.nextLine();

         if (!inputString.isEmpty()) {
            System.out.print("Enter the index to find: ");
            int inputIndex = kbScan.nextInt();
            kbScan.nextLine();

            try {
               char match = inputString.charAt(inputIndex);
               System.out.println(match);
            } catch (IndexOutOfBoundsException e) {
               System.err.print(e.getMessage() + "\nTry again!\n\n");
            }
         }
      } 
   }
}
