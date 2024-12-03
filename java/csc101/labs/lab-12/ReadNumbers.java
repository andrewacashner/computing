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

        System.out.print("Enter the name of an input file in the current directory: ");
        String infileName = kbScan.nextLine();

        System.out.print("Enter the name of an output file in the current directory: ");
        String outfileName = kbScan.nextLine();

        File infile = new File(infileName);

        if (!infile.exists()) {
            System.err.format("Could not open input file %s for reading\n", infileName);
            return;
        }

        int[] inputNums = {};
        try {
            Scanner infileReader = new Scanner(infile);

            int numCount = countIntInputs(new Scanner(infile));

            inputNums = new int[numCount];

            for (int i = 0; infileReader.hasNextInt(); ++i) {
                inputNums[i] = infileReader.nextInt();
            }
        }
        catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
            return;
        }
        catch (NumberFormatException e) {
            System.err.println("Problem reading input data: " 
                    + e.getMessage());
        } 

        // Generate report
        if (inputNums.length == 0) {
            System.err.println("No input numbers stored.");
            return;
        }
        
        String report = numberReport(inputNums);
        System.out.println(report);
       
        // Write to file
        File outfile = new File(outfileName);
        FileOutputStream outfileStream = null; 
        PrintWriter outfileWriter = null;
        // Could I just pass the File to PrintWriter and skip the
        // FileOutputStream?

        try {
            if (!outfile.exists()) {
                outfile.createNewFile();
            }
            
            outfileStream = new FileOutputStream(outfile);
            outfileWriter = new PrintWriter(outfileStream);
            
            outfileWriter.println(report);

            System.out.format("\nReport written to file %s\n", outfileName);
        }
        catch (IOException e) {
            System.err.format("Problem writing to output file %s: %s",
                    outfileName, e.getMessage());
        }
        finally {
            if (outfileWriter != null) {
                outfileWriter.close();
            }
            if (outfileStream != null) {
                outfileStream.close(); 
            }
        }
    }

    private static int countIntInputs(Scanner infileScanner) {
        int numCount = 0;
        while (infileScanner.hasNextInt()) {
            int n = infileScanner.nextInt();
            ++numCount;
        }
        return numCount;
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

