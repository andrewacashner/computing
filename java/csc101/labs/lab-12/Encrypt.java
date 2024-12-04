import java.util.Scanner;
import java.io.*;

/**
 * Encrypt and decrypt a text file. 
 * Encrypt by shifting each character to the right
 * (<code>a-&gt;b</code>, <code>Z-&gt;A</code>).
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/12/04 (CSC 101, Lab 12)
 */
public class Encrypt {
   /**
    * Ask user for the name of input and output files and a choice of
    * whether to encrypt or decrypt the input file; write the results to the
    * output file.
    *
    * @param args Unused command-line arguments
    */
   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);

      System.out.println("FILE ENCRYPTOR");
      String fileContents = requestReadFile(kbScan);
      String processedContents = getEncryptAction(kbScan, fileContents);
      requestWriteFile(kbScan, processedContents);
   }

   // INPUT AND OUTPUT
   /**
    * Get an input file from the user and read its contents into a string.
    * Loop until a valid filename is given.
    *
    * @param userScanner Scanner for reading user input 
    *           (e.g., from System.in)
    * @return String with file contents
    */
   private static String requestReadFile(Scanner userScanner) {
      String fileContents = null;
      StringBuilder buffer = new StringBuilder();

      while (fileContents == null) {
         String infileName = new String();
         System.out.print("Enter the name of the input file: ");
         infileName = userScanner.nextLine();
        
         Scanner reader = null;
         try {
            File infile = new File(infileName);
            reader = new Scanner(infile);
            
            while (reader.hasNextLine()) {
               buffer.append(reader.nextLine() + "\n");
            }

            fileContents = buffer.toString();
         } 
         catch (FileNotFoundException e) {
            System.err.format("Could not open file %s for reading: %s\n", 
                  infileName, e.getMessage());
         }
         finally {
            if (reader != null) {
               reader.close();
            }
         }
      }

      return fileContents;
   }

   /**
    * Request an output file and write the given text to it; loop until a
    * valid file is given.
    *
    * @param userScanner Scanner for reading user input
    * @param text String to write
    */
   private static void requestWriteFile(Scanner userScanner, String text) {
      String outfileName = new String();
      PrintWriter writer = null;
      
      while (writer == null) {
         System.out.print("Enter the name of the output file: ");
         outfileName = userScanner.nextLine();

         try {
            File outfile = new File(outfileName);
            writer = new PrintWriter(outfile);
            writer.print(text);
         } 
         catch (IOException e) {
            System.err.format("Could not open file %s for writing; Try again\n", outfileName);
         }
         finally {
            writer.close();
         }
      }

      System.out.format("Output written to file %s.\n", outfileName);
   }

   // SELECT ENCRYPTION OR DECRYPTION
   /**
    * Ask the user whether to encrypt or decrypt a given string, and return
    * the encrypted or decrypted string.
    *
    * @param reader Scanner for reading user input
    * @param text String to encrypt or decrypt
    * @return Encrypted or decrypted string
    */
   private static String getEncryptAction(Scanner reader, String text) {
      String crypted = null;

      while (crypted == null) {
         System.out.print("Encrypt or decrypt? Enter e or d: ");
         String userAction = reader.nextLine();

         switch (userAction.toLowerCase()) {
            case "e":
               crypted = encrypt(text);
               break;

            case "d":
               crypted = decrypt(text);
               break;

            default:
               System.err.format("Unrecognized command %s; Try again\n", userAction);
         }
      }

      return crypted;
   }

   // ENCRYPTION
   /**
    * Is this character in the given alphabet?
    *
    * @param alphabet List of characters as string
    * @param c Character to find
    * @return True if alphabet contains c
    */
   private static boolean alphabetContains(String alphabet, char c) {
      return alphabet.indexOf(Character.toLowerCase(c)) >= 0;
   }

   /**
    * Return the appropriate alphabet to find this character (upper or
    * lower case).
    *
    * @param alphabet List of characters as string
    * @param c Character to find
    * @return Upper or lowercase version of given alphabet, matching
    * character
    */
   private static String matchAlphabet(String alphabet, char c) {
      String lower = alphabet;
      String upper = lower.toUpperCase();

      return Character.isUpperCase(c) ? upper : lower;
   }

   /**
    * Encode (encrypt or decrypt) a string by shifting each letter
    * value; loop around at end of the alphabet.
    * To encrypt (with param encrypt = true), shift each letter to the
    * right; to decrypt (param encrypt = false), shift to the left.
    *
    * @param src String to encode
    * @param encrypt Boolean: true to encrypt, false to decrypt
    * @return Encoded string
    */
   private static String encode(String src, boolean encrypt) {

      final String ALPHABET = "abcdefghijklmnopqrstuvwxyz";
      final int ALPHABET_LENGTH = ALPHABET.length();

      StringBuilder msg = new StringBuilder();

      for (int i = 0; i < src.length(); ++i) {

         char thisChar = src.charAt(i);
         char newChar;

         if (alphabetContains(ALPHABET, thisChar)) {
            String alphabet = matchAlphabet(ALPHABET, thisChar);
   
            // Find the index of this char, 
            // add (to encrypt) or subtract (to decrypt), 
            // and return the alphabet character at the shifted index.
            int index = alphabet.indexOf(thisChar);
            int increment = encrypt ? 1 : -1;
            int newIndex = (index + increment) % ALPHABET_LENGTH;
            newChar = alphabet.charAt(newIndex);
         
         } else {
            // If this is not an alphabetic character (e.g., punctuation,
            // high Unicode), just copy it
            newChar = thisChar;
         }

         msg.append(newChar);
      }

      return msg.toString();
   }

   /**
    * Encrypt a string.
    *
    * @param src String to encrypt
    * @return Encrypted string
    */
   private static String encrypt(String src) {
      return encode(src, true);
   }

   /**
    * Decrypt a string.
    *
    * @param src String to decrypt
    * @return Decrypted string
    */
   private static String decrypt(String src) {
      return encode(src, false);
   }
}
