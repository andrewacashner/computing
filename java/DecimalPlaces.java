public class DecimalPlaces {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: DecimalPlaces [NUMBER]");
            return;
        }

        double num = Double.parseDouble(args[0]);

        System.out.println(decimalPlaces(num));
    }

    private static int decimalPlaces(double n) {
        final int SIGNIFICANT_DIGITS = 10;

        int decimalPlaces = 0;
        boolean found = false;

        for (int exp = 0; !found && exp < SIGNIFICANT_DIGITS; ++exp) {
            double test = n * Math.pow(10, exp);
            
            if (test - Math.floor(test) < Math.pow(10, -SIGNIFICANT_DIGITS)) {
                found = true;
            } else {
                ++decimalPlaces;
            }
            
            // System.out.format("floor %.20f == %.20f = %b (%d places)\n", 
            //      Math.floor(test), test, Math.floor(test) == test, decimalPlaces);
        }
        return decimalPlaces;
    }
}
