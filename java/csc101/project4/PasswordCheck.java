import java.util.Scanner;

/**
 * Check passwords for validity according to these rules:
 * <ol>
 *   <li>At least 10 characters, not more than 16</li>
 *   <li>Contains at least one of these symbols: 
 *       <code>!, #, $, %, ^, &amp;, *, &lt;, &gt;, ?</code></li>
 *   <li>Contains at least two capital letters</li>
 *   <li>Contains exactly one digit</li>
 *   <li>Contains no spaces</li>
 * </ol>
 *
 * Report the problems if invalid.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/25 (CSC 101, Project 4)
 */
public class PasswordCheck {
  
   /**
    * Ask user for passwords to check and report if they are valid.
    * If not, report the failing conditions.
    * Loop until user exits
    *
    * @param args (Unused) Command-line args
    */
   public static void main(String[] args) {

      Scanner kbScan = new Scanner(System.in);

      System.out.println("PASSWORD CHECKER");
      String input = new String();

      while (!input.equals("q")) {
         System.out.print("\nEnter a password to test (or 'q' to quit): ");
         input = kbScan.nextLine();

         if (input.equals("q")) {
            continue;
         }

         Password password = new Password(input);
         password.checkPassword();
      }

      System.out.println("Thank you for using the Password Checker.");
   }
}

/**
 * Stores and validates a password.
 */
class Password {
   /** The word as input by the user */
   private String password;

   /** Minimum length of password */
   final static int PASSWORD_LENGTH_MIN = 10;

   /** Maximum length of password */
   final static int PASSWORD_LENGTH_MAX = 16;

   /** List of required symbols */
   final static String SYMBOLS = "!#$%^&*<>?";

   /** List of required digits */
   final static String DIGITS = "0123456789";

   /** Minimum symbols from list required */
   final static int SYMBOL_MIN = 1;

   /** Minimum number of numerals */
   final static int DIGIT_MIN = 1;

   /** Minimum number of capital (uppercase) letters */
   final static int CAPITAL_MIN = 2;

   
   /** Create a new password from user input (Not validated) */
   public Password(String password) {
      this.password = password;
   }

   /** 
    * Is the password in the right length range between PASSWORD_LENGTH_MIN
    * and PASSWORD_LENGTH_MAX?
    *
    * @return True if length is valid
    */
   private boolean validContentLength() {
      int length = this.password.length();
      return length >= PASSWORD_LENGTH_MIN && length <= PASSWORD_LENGTH_MAX;
   }

   /**
    * Are there the required number of capital letters?
    *
    * @return True if valid number
    */
   private boolean validContentCapitals() {
      int capitalCount = 0;
      for (int i = 0; i < this.password.length(); ++i) {
         if (Character.isUpperCase(this.password.charAt(i))) {
            ++capitalCount;
         }
      }
      return capitalCount >= CAPITAL_MIN;
   }

   /** 
    * Does a given string contain a given character? DIY version of
    * existing library functions.
    *
    * @param source String to search
    * @param c Character to search for
    * @return True if the string contains the character
    */
   private static boolean contains(String source, char c) {
      boolean found = false;
      for (int i = 0; i < source.length() && !found; ++i) {
         found = source.charAt(i) == c;
      }
      return found;
   }

   /**
    * Does a given string contain at least a given number of characters
    * from a set held in a second string?
    *
    * @param source String to check
    * @param matchSet String with set of characters to look for
    * @param minMatches Minimum number of matches to find
    * @return True if number of found matches exceeds minMatches
    */
   private static boolean containsAtLeast(String str, String matchSet, 
         int minMatches) {

      int foundMatches = 0;
      for (int i = 0; i < str.length(); ++i) {
         if (contains(matchSet, str.charAt(i))) {
            ++foundMatches;
         }
      }
      return foundMatches >= minMatches;
   }

   /** 
    * Does this password contain enough symbols from the list?
    *
    * @return True if it does contain enough symbols
    */
   private boolean validContentSymbols() {
      return this.containsAtLeast(this.password, this.SYMBOLS, 
            this.SYMBOL_MIN);
   }

   /**
    * Does this password contain enough digits (numerals)?
    *
    * @return True if it has enough
    */
   private boolean validContentDigits() {
      return this.containsAtLeast(this.password, this.DIGITS, 
            this.DIGIT_MIN);
   }

   /**
    * Does this password contain no spaces?
    *
    * @return True if there are no spaces
    */
   private boolean validContentSpaces() {
      return this.password.indexOf(' ') == -1;
   }

   /**
    * Is the password valid?
    *
    * @return True if valid
    */
   public boolean isValid() {
      return this.validContentLength() &&
         this.validContentCapitals() &&
         this.validContentSymbols() &&
         this.validContentDigits() &&
         this.validContentSpaces();
   }

   /**
    * Add a plural s if the quanitity is plural.
    *
    * @param quantity Integer number of items
    * @return String with plural s, or blank
    */
   static String maybePlural(int quantity) {
      return quantity > 1 ? "s" : "";
   }

   /**
    * Check each validity test and print a report of each part missed.
    */
   public void checkPassword() {

      boolean valid = true;

      if (!this.validContentLength()) {
         valid = false;
         System.out.format("Must be between %d and %d characters long\n", 
               PASSWORD_LENGTH_MIN, PASSWORD_LENGTH_MAX);
      }

      if (!this.validContentCapitals()) {
         valid = false;
         System.out.format(
               "Must contain at least %d uppercase letter%s\n", 
               this.CAPITAL_MIN, this.maybePlural(this.CAPITAL_MIN));
      } 

      if (!this.validContentSymbols()) {
         valid = false;
         System.out.format(
               "Must contain at least %d of these symbols: %s\n", 
               this.SYMBOL_MIN, this.SYMBOLS);
      } 

      if (!this.validContentDigits()) {
         valid = false;
         System.out.format("Must contain at least %d numeral%s\n", 
               this.DIGIT_MIN, this.maybePlural(this.DIGIT_MIN));
      } 

      if (!this.validContentSpaces()) {
         valid = false;
         System.out.print("Cannot contain spaces\n");
      } 

      if (valid) {
         System.out.print("Success: Password is valid!\n");
      }
   }
}
