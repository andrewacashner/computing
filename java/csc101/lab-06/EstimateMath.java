/**
 * A library for math using estimates.
 *
 * (I know the assignment says this should contain "only two static methods"
 * but since both round their parameters it made sense to extract that into
 * its own method.)
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/10 (CSC 101, Lab 5)
 */
public class EstimateMath {

    /**
     * Round an integer to the nearest ten.
     *
     * @param num The integer
     *
     * @return The rounded value
     */
    static int roundNearestTen(int num) {
        int rounded = (int)Math.round((double)num / 10) * 10;
        return rounded;
    }

    /** 
     * Given two integer values, round them to the nearest 10 and then add
     * them.
     * 
     * @param numA First integer
     * @param numB Second integer
     *
     * @return Estimated sum
     */
    public static int estimateAdd(int numA, int numB) {
        return roundNearestTen(numA) + roundNearestTen(numB);
    }

    /** 
     * Given two integer values, round them to the nearest 10 and then
     * subtract them.
     * 
     * @param numA First integer
     * @param numB Second integer
     *
     * @return Estimated difference
     */
    public static int estimateSubtract(int numA, int numB) {
        return roundNearestTen(numA) - roundNearestTen(numB);
    }
}
