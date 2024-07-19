/* First Java program
 * Andrew Cashner
 * 2024/07/19
 */

import java.util.Random;

public class HelloWorld {
    static String randomName(Random generator) {
        String[] names = {
            "Elvis",
            "Jelly Roll",
            "Louis",
            "Ella",
            "Candice",
            "Scott",
            "Wanda",
            "Ruth",
            "Frank",
            "Kittie"
        };
        int index = generator.nextInt(names.length);
        return names[index];
    }

    public static void main(String[] args) {
        Random generator = new Random();
        String name = randomName(generator);
        System.out.format("Hello, %s!\n", name);
    }
}
