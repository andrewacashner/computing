/* Transpose a given pitch by a given interval 
 * Andrew Cashner, 2024/09/11
 */

class Transpose {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java Transpose PITCH INTERVAL");
            System.exit(1);
        }

        String pitchStr = args[0];
        String intervalStr = args[1];

        Pitch origin = new Pitch(pitchStr);
        System.out.format("%s + %s = ?\n", origin, intervalStr);

    }
}
