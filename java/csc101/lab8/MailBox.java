import java.util.Scanner;

/**
 * Open and close mailboxes in a pattern.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/24 (CSC 101, Lab 8)
 */
public class MailBox {
    /** 
     * I just can't get my head around this without using these constants.
     */
    final static boolean CLOSED = true;
    final static boolean OPEN = false;

    /** 
     * Ask user to specify a number of mailboxes, then programmatically
     * toggle the open/close states of those mailboxes in a pattern.
     *
     * @param args (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        // Get input
        Scanner kbScan = new Scanner(System.in);

        System.out.println("MAILBOX: Open and close some mailboxes");
        System.out.print("How many mailboxes are there? ");
        int maxBoxes = kbScan.nextInt();

        // Mess with boxes: Start with all closed, then toggle them in a
        // pattern
        boolean[] closedMailBoxes = new boolean[maxBoxes]; 
        closeAllBoxes(closedMailBoxes);
        toggleBoxes(closedMailBoxes);

        // Report results
        String boxList = listClosedBoxes(closedMailBoxes);
        System.out.format("These boxes are now closed: %s\n", boxList);
    }

    /**
     * Set all boxes in the array to CLOSED.
     *
     * @param boxes Array of booleans (OPEN or CLOSED)
     */
    public static void closeAllBoxes(boolean[] boxes) {
        for (int i = 0; i < boxes.length; ++i) {
            boxes[i] = CLOSED;
        }
    }

    /**
     * For a given n, start at n and toggle every nth box in the array.
     *
     * @param boxes Array of booleans (OPEN or CLOSED)
     * @param interval n
     */
    public static void toggleEveryNthBox(boolean[] boxes, int interval) {
        for (int i = interval - 1; i < boxes.length; i += interval) {
            boxes[i] = !boxes[i];
        }
    }

    /**
     * For a range of n from 2 to the number of boxes, toggle every nth box
     * successively.
     *
     * @param boxes Array of booleans (OPEN or CLOSED)
     */
    public static void toggleBoxes(boolean[] boxes) {
        for (int i = 2; i < boxes.length; ++i) {
            toggleEveryNthBox(boxes, i);
        }
    }

    /**
     * Is this box closed?
     *
     * @param boxes Array of booleans (OPEN or CLOSED)
     * @return True if the box is CLOSED
     */
    public static boolean isClosed(boolean box) {
        return box == CLOSED;
    }

    /**
     * Return a list of the 1-indexed positions of those boxes in the array
     * that are closed, omitting open boxes.
     *
     * @param boxes Array of booleans (OPEN or CLOSED)
     * @return List of array indices formatted like Arrays.toString()
     */
     public static String listClosedBoxes(boolean[] boxes) {
        String boxList = new String();

        for (int i = 0; i < boxes.length; ++i) {
            if (isClosed(boxes[i])) {
                String delimiter = i > 0 ? ", " : "";
                boxList += String.format("%s%d", delimiter, i + 1);
            }
        }

        return String.format("[%s]", boxList);
    }
}
