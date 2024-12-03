import java.util.Scanner;
import java.io.*;
import java.util.Arrays;

/**
 * Read numbers from a file and report the following info:
 * <ol>
 *   <li>How many numbers were read</li>
 *   <li>Maximum value</li>
 *   <li>Minimum value</li>
 *   <li>Average</li>
 *   <li>Standard deviation</li>
 * </ol>
 * Display the output on screen and write it to an output file.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/12/03 (CSC 101, Lab 12)
 */
public class ReadNumbers {
   /**
    * Ask user for name of input and output files; read and analyze the
    * input file and write the results to both console and output file.
    *
    * @param args Unused command-line arguments
    */
   public static void main(String[] args) throws IOException {
      Scanner kbScan = new Scanner(System.in);
      System.out.println("NUMBER CRUNCHER: Report info on numbers from a file");

      // Read and store input
      int inputNums[] = readIntInputs(kbScan);

      if (inputNums == null || inputNums.length == 0) {
         System.err.println("No input numbers stored.");
         return;
      }

      System.out.print(Arrays.toString(inputNums));
      // Generate report
      String report = numberReport(inputNums);
     
      // Print to stdout and file
      writeToFile(kbScan, report);
      System.out.println(report);
   }

   private static int countIntInputs(Scanner fileScanner) {
      int numCount = 0;
      while (fileScanner.hasNextInt()) {
         int n = fileScanner.nextInt();
         ++numCount;
      }
      return numCount;
   }

   private static int[] readIntInputs(Scanner userScanner) 
         throws FileNotFoundException {

      int[] inputNums = {};
      Scanner fileScanner = null;

      while (fileScanner == null) {
         System.out.print("Enter the name of the input file: ");
         String infileName = userScanner.nextLine();

         try {
            File infile = new File(infileName);
            fileScanner = new Scanner(infile);

            int numCount = countIntInputs(fileScanner);
            inputNums = new int[numCount];

            System.err.format("Found %d nums\n", numCount);

            // TODO START not getting anything
            for (int i = 0; fileScanner.hasNextInt(); ++i) {
               System.err.println("Found one");
               inputNums[i] = fileScanner.nextInt();
            }
         } 
         catch (FileNotFoundException e) {
            System.err.format("Could not open file %s for reading: %s\n", infileName, e.getMessage());
         }
         finally {
            if (fileScanner != null) {
               fileScanner.close();
            }
         }
      }

      return inputNums;
   }

   private static void writeToFile(Scanner userScanner, String text) {

      PrintWriter writer = null;

      while (writer == null) {
         System.out.print("Enter the name of an output file in the current directory: ");
         String outfileName = userScanner.nextLine();

         try {
            File outfile = new File(outfileName);
            if (!outfile.exists()) {
               outfile.createNewFile();
            }

            writer = new PrintWriter(outfile);
            writer.println(text);

            System.out.format("Report written to file %s\n", outfileName);
         }
         catch (IOException e) {
            System.err.format("Problem writing to output file %s: %s",
                  outfileName, e.getMessage());
         }
         finally {
            if (writer != null) {
               writer.close();
            }
         }
      }
   }

   private static int maximum(int[] nums) {
      int max = nums[0];
      for (int num: nums) {
         max = Math.max(num, max);
      }
      return max;
   }

   private static int minimum(int[] nums) {
      int min = nums[0];
      for (int num: nums) {
         min = Math.min(num, min);
      }
      return min;
   }

   private static double average(int[] nums) {
      int sum = 0;
      for (int num: nums) {
         sum += num;
      }
      return sum / nums.length;
   }

   private static double stdDeviation(int[] nums) {
      double avg = average(nums);
      double numerator = 0;
      for (int num: nums) {
         numerator += Math.pow(num - avg, 2);
      }
      return Math.sqrt(numerator / nums.length);
   }

   private static String numberReport(int[] nums) {
      int max = maximum(nums);
      int min = minimum(nums);
      double average = average(nums);
      double stdDeviation = stdDeviation(nums);

      String report = String.format(
            "Read from file: %d values\n" +
            "     Maximum value = %d\n" +
            "     Minimum value = %d\n" + 
            "     Average value = %.2f\n" +
            "     Standard Deviation = %.2f",
            nums.length, max, min, average, stdDeviation);

      return report;
   }

}

