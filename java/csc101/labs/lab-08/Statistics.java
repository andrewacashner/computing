import java.util.Scanner;

/**
 * Calculate average and standard deviation for a given set of numbers.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/24 (CSC 101, Lab 8)
 */
public class Statistics {
    /** 
     * Given a list of double values, calculate the average and standard
     * deviation and report the results.
     *
     * @param args (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        // Get input
        Scanner kbScan = new Scanner(System.in);

        System.out.println("STATISTICS: Calculate Average and Standard Deviation");
        
        System.out.print("How many values are in the set? ");
        int maxValues = kbScan.nextInt();

        double[] values = new double[maxValues];

        System.out.print("Enter the list of values: ");
        for (int i = 0; i < maxValues; ++i) {
            values[i] = kbScan.nextDouble();
        }

        // Calculate
        double average = average(values);
        double deviation = stdDeviation(values);

        // Return output
        System.out.print(resultsTable(average, deviation));
    }

    /**
     * Calculate average (mean) of a set of values.
     *
     * @param values Array of doubles
     * @return Average
     */
    public static double average(double[] values) {
        double average;
        double sum = 0;
        
        for (double thisValue: values) {
            sum += thisValue;
        }
        
        average = sum / values.length;

        return average;
    }

    /**
     * Calculate standard deviation of a set of values.
     *
     * @param values Array of doubles
     * @return Standard deviation
     */
    public static double stdDeviation(double[] values) {
        double deviation;
        double average = average(values);
        double numerator = 0;

        for (double thisValue: values) {
            numerator += Math.pow(thisValue - average, 2);
        }

        deviation = Math.sqrt(numerator / values.length);
            
        return deviation;
    }

    /**
     * Create a table showing the average and standard deviation.
     *
     * @param average   Average
     * @param deviation Standard deviation
     * @return Table as string
     */
    public static String resultsTable(double average, double deviation) {
        // Set column width to hold widest value
        // TODO surely there is a more elegant way to do this
        int width = "Standard deviation".length() + 2;
        String formatSpec = "%-" + String.format("%d", width) + "s %f\n";

        String table = 
            String.format(formatSpec, "Average", average) + 
            String.format(formatSpec, "Standard Deviation", deviation);

        return table;
    }
}
