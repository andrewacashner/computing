import java.util.Scanner;
import java.util.InputMismatchException;

public class ReadFive {
   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);
      System.out.println("READ FIVE: Find the average of five integers");

      int[] inputInts = new int[5];

      for (int i = 0; i < inputInts.length; ++i) {
         try {
            System.out.printf("Enter integer #%d: ", i + 1);
            inputInts[i] = readOneInt(kbScan);

         } catch (InputMismatchException e) {
            // Reporting the invalid input string also clears the buffer
            System.err.printf("Invalid input \"%s\": Try again!\n", 
                  kbScan.nextLine());
            --i;
         } catch (IllegalArgumentException e) {
            System.err.print(e.getMessage() + "\nTry again!\n");
            kbScan.nextLine(); // Clear the buffer
            --i;
         }
      }

      double average = average(inputInts);
      System.out.printf("Average: %f\n", average);
   }

   private static int readOneInt(Scanner kbScan) 
         throws IllegalArgumentException {

         int inputInt;
         inputInt = kbScan.nextInt();
         if (inputInt <= 0) {
            throw new IllegalArgumentException(
                  String.format("Input %d out of range: " +
                     "inputs must be positive integers", inputInt));
         }

         return inputInt;
   }

   private static double average(int[] nums) {
      int sum = 0;
      for (int i = 0; i < nums.length; ++i) {
         sum += nums[i];
      }
      return sum / nums.length;
   }
}
