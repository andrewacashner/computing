public class PolynomialRun {
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: PolynomialRun ACTION \"c1 c2 c3 ...\" [ARGS]");
            return;
        }
        String action = args[0];
        String coefficients = args[1];
        Polynomial p = new Polynomial(coefficients);

        switch (action) {
            case "display":
                System.out.println(p);
                break;
            case "root":
                if (args.length != 3) {
                    System.err.println("Root requires additional argument for initial guess");
                    return;
                }
                try {
                    double guess = Double.parseDouble(args[2]);
                    double root = p.findRoot(guess);
                    System.out.println(root);
                } catch (IllegalStateException e) {
                    System.err.println(e.getMessage());
                } catch (Exception e) {
                    System.err.printf("Could not parse input \"%s\" to double\n", args[2]);
                    System.err.println(e.getMessage());
                    return;
                }
                break;
            default: 
                System.err.printf("Unknown action %s\n", action);
                break;
        }
    }
}
