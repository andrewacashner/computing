//2024/11/08

import java.util.List;
import java.util.Arrays;
import java.util.function.Predicate;

class Lambda {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: java Lambda PASSWORD");
            return;
        }

        List<String> passwordLs = Arrays.asList(args);

        Predicate<String> longEnough = s -> s.length() > 10;
        
        // Next step: Map the function instead ?
        // or filter by the predicate and return only valid pws
        // or partition by the predicate and return list of valid and invalid
        for (String password: passwordLs) {
            if (longEnough.test(password)) {
                System.out.printf("\nValid password '%s'!\n", password);
            } else {
                System.out.printf("\nInvalid password '%s'!\n", password);
                System.out.print("Passwords be at least 10 characters long\n");
            }
        }
    }
}

