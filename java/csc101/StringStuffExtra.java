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
    static String tableRow(String key, String value) {
        return String.format("%-40s| %s\n", key, value);
    }

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        Random randGen = new Random();

        System.out.println("STRING STUFF");
        System.out.println("Enter a multiple-word phrase:");

        String phrase = input.nextLine();

        int length = phrase.length();
        String substring = phrase.substring(1, length - 1);

        int randomIndex = randGen.nextInt(length);
        String randomLetter = phrase.substring(randomIndex, randomIndex + 1);
        // better: char randomLetter = phrase.charAt(randomIndex);

        int firstSpaceIndex = phrase.indexOf(" ");

        String rule = new String(        "--------------------------------------------------------------");

        System.out.println(rule);
        System.out.printf(tableRow(
                    "Length", 
                    String.format("%d", length)));
        System.out.printf(tableRow(
                    "Substring minus first and last letters", 
                    String.format("\"%s\"", substring)));
        System.out.printf(tableRow(
                String.format("Letter at random index %d", randomIndex), 
                String.format("\'%s\'", randomLetter)));
        System.out.printf(tableRow(
                "Index of first space character",
                String.format("%d", firstSpaceIndex)));
/*
        System.out.printf("%-40s| %d\n", "Length", length);
        System.out.printf("%-40s| \"%s\"\n", 
                "Substring minus first and last letters", substring);
        System.out.printf("%-40s| %d\n", "Random index", randomIndex);
        System.out.printf("%-40s| \'%s\'\n", 
                "Letter at random index", randomLetter);
        System.out.printf("%-40s| %d\n", 
                "Index of first space character", firstSpaceIndex);
                */
        System.out.println(rule);
    }
}
