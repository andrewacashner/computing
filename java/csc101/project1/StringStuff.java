/* StringStuff
 *
 * Given a multiple-word phrase, report some information about the string.
 *
 * Andrew A. Cashner, 2024/09/10
 * For CSC101, Assignment 1
 */

import java.util.Scanner;
import java.util.Random;

class StringStuff {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        Random randGen = new Random();

        System.out.println("STRING STUFF");
        System.out.println("See key info about a string\n");
        System.out.println("Enter a multiple-word phrase:");

        String phrase = input.nextLine();

        int length = phrase.length();
        String substring = phrase.substring(1, length - 1);

        int randomIndex = randGen.nextInt(length);
        String randomLetter = phrase.substring(randomIndex, randomIndex + 1);

        int firstSpaceIndex = phrase.indexOf(" ");

        String rule = new String(        "--------------------------------------------------------------");

        System.out.println(rule);
        System.out.printf("%-40s| %d\n", "Length", length);
        System.out.printf("%-40s| \"%s\"\n", 
                "Substring minus first and last letters", substring);
        System.out.printf("%-40s| \'%s\'\n",
                String.format("Letter at random index %d", randomIndex), 
                randomIndex);
        System.out.printf("%-40s| %d\n", 
                "Index of first space character", firstSpaceIndex);
        System.out.println(rule);
    }
}
