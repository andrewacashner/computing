import java.util.Scanner;
import java.io.*;

/**
 * Encrypt and decrypt a text file. 
 * Encrypt by shifting each character to the right
 * (<code>a-&gt;b</code>, <code>Z-&gt;A</code>).
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/12/03 (CSC 101, Lab 12)
 */
public class Encrypt {
   public static void main(String[] args) {
      Scanner kbScan = new Scanner(System.in);

      System.out.println("FILE ENCRYPTOR");

      String fileContents = requestReadInputFile(kbScan);
      PrintWriter writer = setupWriter(kbScan);
      
      String processedContents = getEncryptAction(kbScan, fileContents);

      writeToFile(writer, processedContents);
   }

   private static String requestReadInputFile(Scanner reader) {
      String fileContents = null;

      while (fileContents == null) {
         String infileName = new String();
         System.out.print("Enter the name of the input file: ");

         infileName = reader.nextLine();
         try {
            fileContents = fileToString(infileName);
         } 
         catch (FileNotFoundException e) {
            System.err.format("Could not open file %s for reading: %s\n", 
                  infileName, e.getMessage());
         }
      }

      return fileContents;
   }

   private static String fileToString(String infileName) 
         throws FileNotFoundException {

      File infile = new File(infileName);
      Scanner reader = new Scanner(infile);
      StringBuilder buffer = new StringBuilder();

      while (reader.hasNextLine()) {
         buffer.append(reader.nextLine() + "\n");
      }

      reader.close();

      return buffer.toString();
   }

   private static PrintWriter setupWriter(Scanner reader) {
      String outfileName = new String();
      PrintWriter writer = null;
      
      while (writer == null) {
         System.out.print("Enter the name of the output file: ");
         
         try {
            outfileName = reader.nextLine();
            File outfile = new File(outfileName);
            writer = new PrintWriter(outfile);
         } 
         catch (IOException e) {
            System.err.format("Could not open file %s for writing; Try again\n", outfileName);
         }
      }

      return writer;
   }

   private static String getEncryptAction(Scanner reader, String text) {
      String processedContents = null;

      while (processedContents == null) {
         System.out.print("Encrypt or decrypt? Enter E or D: ");
         String userAction = reader.nextLine();

         switch (userAction.toUpperCase()) {
            case "E":
               processedContents = encrypt(text);
               break;

            case "D":
               processedContents = decrypt(text);
               break;

            default:
               System.err.format("Unrecognized command %s; Try again\n", userAction);
         }
      }

      return processedContents;
   }

   private static void writeToFile(PrintWriter writer, String text) {
      writer.print(text);
      writer.close();
      System.out.println("Output written to file.");
   }

   private static boolean alphabetContains(String alphabet, char c) {
      return alphabet.indexOf(Character.toLowerCase(c)) >= 0;
   }

   private static String matchAlphabet(String alphabet, char c) {
      String lower = alphabet;
      String upper = lower.toUpperCase();

      return Character.isUpperCase(c) ? upper : lower;
   }

   private static String encode(String src, boolean encrypt) {

      final String ALPHABET = "abcdefghijklmnopqrstuvwxyz";
      final int ALPHABET_LENGTH = ALPHABET.length();

      StringBuilder msg = new StringBuilder();

      for (int i = 0; i < src.length(); ++i) {

         char thisChar = src.charAt(i);
         char newChar;

         if (alphabetContains(ALPHABET, thisChar)) {
            String alphabet = matchAlphabet(ALPHABET, thisChar);
      
            int index = alphabet.indexOf(thisChar);
            int increment = encrypt ? 1 : -1;

            int newIndex = (index + increment) % ALPHABET_LENGTH;
            newChar = alphabet.charAt(newIndex);
         
         } else {
            newChar = thisChar;
         }

         msg.append(newChar);
      }

      return msg.toString();
   }

   private static String encrypt(String src) {
      return encode(src, true);
   }

   private static String decrypt(String src) {
      return encode(src, false);
   }
}
