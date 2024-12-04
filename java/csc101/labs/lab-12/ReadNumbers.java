import java.util.Scanner;
import java.io.*;

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
 * @version 2024/12/04 (CSC 101, Lab 12)
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

      // Read, store, and check input
      int inputNums[] = requestReadIntInputs(kbScan);

      if (inputNums == null || inputNums.length == 0) {
         System.err.println("No input numbers stored.");
         return;
      }

      // Generate report
      String report = numberReport(inputNums);
     
      // Print to stdout and file
      requestWriteFile(kbScan, report);
      System.out.println(report);
   }

   // INPUT AND OUTPUT
   /**
    * Ask the user for the name of an input file and read a series of int
    * inputs from that file.
    * If the given file is not found, ask for another name until a good one
    * is given.
    * To create the array we have to first count the number of int values
    * readable from the file.
    *
    * @param userScanner Scanner to read values from the user 
    *       (e.g., from System.in)
    * @return Array of integers read from file
    * @throws FileNotFoundException
    */
   private static int[] requestReadIntInputs(Scanner userScanner) 
         throws FileNotFoundException {

      int[] inputNums = {};
      Scanner fileReader = null;

      while (fileReader == null) {
         System.out.print("Enter the name of the input file: ");
         String infileName = userScanner.nextLine();

         Scanner fileCounter = null;
         
         try {
            File infile = new File(infileName);
            fileCounter = new Scanner(infile);
            fileReader = new Scanner(infile);

            // Just count int values in file
            int numCount = 0;
            while (fileCounter.hasNextInt()) {
               fileCounter.nextInt();
               ++numCount;
            }

            // Now populate the array
            inputNums = new int[numCount];

            for (int i = 0; fileReader.hasNextInt(); ++i) {
               inputNums[i] = fileReader.nextInt();
            }
         } 
         catch (FileNotFoundException e) {
            System.err.format("Could not open file %s for reading: %s\n", infileName, e.getMessage());
         }
         finally {
            if (fileCounter != null) {
               fileCounter.close();
            }
            if (fileReader != null) {
               fileReader.close();
            }
         }
      }

      return inputNums;
   }

   /**
    * Ask the user for the name of an output file, creating a new file if the
    * given one doesn't exist, and write the given text to that file.
    *
    * @param userScanner Scanner to read user input
    * @param text String to write
    */
   private static void requestWriteFile(Scanner userScanner, String text) {

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

   // GENERATE A REPORT 
   /**
    * Return a two-element array containing the minimum and maximum values in
    * an array of integers.
    *
    * @param nums Array of integers
    * @return Minimum and maximum in two-element array
    */
   private static int[] minMax(int[] nums) {
      int min = nums[0];
      int max = nums[0];
      for (int num: nums) {
         min = Math.min(num, min);
         max = Math.max(num, max);
      }
      int[] minMax = { min, max };
      return minMax;
   }

   /**
    * Calculate the average (mean) of an array of integers.
    *
    * @param nums Array of integers
    * @return Double value of average
    */
   private static double average(int[] nums) {
      int sum = 0;
      for (int num: nums) {
         sum += num;
      }
      return sum / nums.length;
   }

   /**
    * Calculate the standard deviation of an array of integers.
    *
    * @param nums Array of integers
    * @return Double value of standard deviation
    */
   private static double stdDeviation(int[] nums) {
      double avg = average(nums);
      double numerator = 0;
      for (int num: nums) {
         numerator += Math.pow(num - avg, 2);
      }
      return Math.sqrt(numerator / nums.length);
   }

   /**
    * Generate a report showing length, minimum, maximum, average, and
    * standard deviation of an array of integers.
    *
    * @param nums Array of integers
    * @return String with report on the data
    */
   private static String numberReport(int[] nums) {
      int[] minMax = minMax(nums);
      double average = average(nums);
      double stdDeviation = stdDeviation(nums);

      String report = String.format(
            "Read from file: %d values\n" +
            "     Maximum value = %d\n" +
            "     Minimum value = %d\n" + 
            "     Average value = %.2f\n" +
            "     Standard Deviation = %.2f",
            nums.length, 
            minMax[1],
            minMax[0],
            average, 
            stdDeviation);

      return report;
   }

}

