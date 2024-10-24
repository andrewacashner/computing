import java.util.Scanner;

/**
 * Record daily expenses for a single week and output them in a table with
 * rows of minimal length.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/24 (CSC 101, Lab 8)
 */
public class Expenses {
    /** 
     * Record daily expenses for a week.
     *
     * Ask the user for daily expenses for each day of a week, Monday through
     * Friday.
     * Store these in an array with rows no longer than they need to be and
     * output them in a formatted table.
     *
     * @param args (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        final int MAX_DAILY_EXPENSES = 20;

        // Get input, store values temporarily for each day
        Scanner kbScan = new Scanner(System.in);
        System.out.println("EXPENSE ACCOUNTANT: Record a week's expenses");

        String[] days = { 
            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" 
        };
        
        double[][] expenses = recordExpenses(days, kbScan,
                MAX_DAILY_EXPENSES);

        // Print the array in a formatted table
        System.out.format("\n%s\n", expenseTable(days, expenses));
    }

    /**
     * Get user input for daily expenses and record it in a 2D matrix.
     *
     * @param days Array of Strings with names of the days
     * @param expenses 2D Array of doubles for expenses by day
     * @param scanner Scanner object
     */
    public static double[][] recordExpenses(String[] days, 
            Scanner scanner, int dailyMax) {

        double[][] expenses = new double[days.length][];

        // Store values in an array with rows of proper length
        // First store in temporary array, then use to that to calculate
        // needed length of expenses array and populate the values.
        for (int dayIndex = 0; dayIndex < days.length; ++dayIndex) {

            System.out.format("== %s ==\n", days[dayIndex].toUpperCase());
            System.out.format("Enter expenses for %s (Enter -1 after the last value).\n", days[dayIndex]);

            double[] todaysExpenses = new double[dailyMax];

            double thisExpense = 0;
            int inputCount = 0;
            while (inputCount < dailyMax && thisExpense != -1) {
                thisExpense = scanner.nextDouble();

                if (thisExpense != -1) {
                    todaysExpenses[inputCount] = thisExpense;
                    ++inputCount;
                }
            }

            // Copy today's values to destination array
            if (inputCount == 0) {
                // If there were no expenses, just add a row with a single
                // entry (zero by default)
                expenses[dayIndex] = new double[1];
            } else {
                expenses[dayIndex] = new double[inputCount];
                for (int i = 0; i < inputCount; ++i) {
                    expenses[dayIndex][i] = todaysExpenses[i];
                }
            }
        }

        return expenses;
    }


    /**
     * Create a formatted table of expenses per day.
     *
     * @param days String array with names of the days 
     *                  (Should match the rows of the expenses parameter)
     * @param expenses 2D Matrix of expenses 
     *                  (rows = days, columns = number of expenses 
     *                  with varying length per row)
     *
     * @return String with formatted table
     */
    public static String expenseTable(String[] days, double[][] expenses) {
        String table = new String("WEEKLY EXPENSES\n");

        for (int i = 0; i < expenses.length; ++i) {
            if (i < days.length) {
                table += String.format("%-10s ", days[i]);
            }
            for (int j = 0; j < expenses[i].length; ++j) {
                table += String.format("%10.2f ", expenses[i][j]);
            }
            table += "\n";
        }

        return table;
    }

}

