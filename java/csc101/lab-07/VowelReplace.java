import java.util.Scanner;

/**
 * Replace all the vowels in a given string with '#'.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/17 (CSC 101, Lab 7)
 */
public class VowelReplace {
    /** 
     * Read an input string, replace the vowels, print the result.
     *
     * @param args (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        final char REPLACEMENT = '#';

        Scanner kbScan = new Scanner(System.in);
        System.out.println("VOWEL REPLACER");
        System.out.println("Enter a phrase and I will replace the vowels:");
        String input = kbScan.nextLine();

        System.out.println(replaceVowels(input, REPLACEMENT));
    }

    /** 
     * Is this character a vowel (a, e, i, o, or u)? Ignore case.
     *
     * @param match Character to test
     * @return Boolean: True if it is a vowel
     */
    static boolean isVowel(char match) {
        final String vowels = new String("aeiou");
        boolean found = false;

        for (int i = 0; i < vowels.length() && !found; ++i) {
            found = vowels.charAt(i) == Character.toLowerCase(match);
        }
        return found;
    }
    
    /** 
     * Replace all the vowels in the input with a given character.
     *
     * @param source String to remove vowels from
     * @param replacement Replacement character
     *
     * @return String with replaced vowels
     */
    public static String replaceVowels(String source, char replacement) {
        StringBuilder modified = new StringBuilder();

        for (int i = 0; i < source.length(); ++i) {
            char thisChar = source.charAt(i);
            char nextChar = isVowel(thisChar) ? replacement : thisChar;
            modified.append(nextChar);
        }

        return modified.toString();
    }
}


