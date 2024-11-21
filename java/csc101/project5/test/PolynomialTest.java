public class PolynomialTest {

   public static void main(String[] args) {
      if (args.length < 1) {
         System.err.println("Usage: java Polynomial \"c0 c1 c2 c3 ...\"");
         return;
      }

      String input = args[0];
      Polynomial fn;
      
      try {
         fn = new Polynomial(input);
         System.out.printf("Created polynomial %s\n", fn);

      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;
      }

      for (double x = 0; x < 5.0; ++x) {
         System.out.printf("Evaluate f(%.0f) = %.4f\n", x, fn.evaluate(x));
      }

      try {
         Polynomial empty = new Polynomial();
         System.out.println(empty.toString());

         Polynomial a = new Polynomial("1 2 3");
         Polynomial b = new Polynomial("1 1 1 1");
         Polynomial c = a.add(b);
         System.out.printf("Add:\n     %s\n   + %s\n   = %s\n", a, b, c);
      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;
      }

      Polynomial d = fn.derivative();
      System.out.printf("Derivative of user fn: %s\n", d);

      try {
         Polynomial e = new Polynomial("-4.5 1.5 1");
         double root = e.findRoot(5);
         System.out.printf("Root of polynomial {%s} ≈ %.6f\n", e, root);

      } catch (NumberFormatException e) {
         System.err.println(e.getMessage());
         return;

      } catch (IllegalStateException e) {
         System.err.println(e.getMessage());
         return;
      }

   }
}
