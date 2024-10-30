import java.util.Scanner;

/**
 * Hangman Game (without the hangman!)
 *
 * Given a mystery word and a maximum number of wrong guesses, run a word
 * guessing game where the user guesses one letter at a time.
 *
 * We implement this using two arrays of boolean values matching the alphabet
 * letters a through z. The first letter catalog records letters in the
 * mystery word that still remain to be found; as each letter is found, those
 * entries are marked false. The second letter catalog records the letters
 * that have already been guessed; as each letter is guessed, that entry is
 * marked true.
 * The user wins when there are no more true values in the lettersToFind
 * catalog, because all have been found.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/30 (CSC 101, Project 4)
 */
public class Hangman {
   /** 
    * Hangman game.
    * Ask user for secret word and number of allowed wrong guesses.
    * In the main game loop, set up the game board and prompt the user to
    * guess, check the answer, and update the display with the result, until
    * the player guesses correctly or has too many wrong guesses.
    *
    * @param args (Unused) Command-line arguments
    */
   public static void main(String[] args) {
      // Get answer from user
      // Get number of wrong guesses allowed from user
      Scanner kbScan = new Scanner(System.in);

      System.out.println("========== HANGMAN ==========");
      System.out.print("Enter the secret word (or phrase): ");
      String answer = kbScan.nextLine();
      answer = answer.toLowerCase();

      System.out.print("Enter the maximum number of wrong guesses: ");
      int maxWrongGuesses = kbScan.nextInt();

      int wrongGuesses = 0;

      String input = new String();
      boolean[] lettersToFind = letterCatalog(answer);
      boolean[] lettersGuessed = letterCatalog();
     
      // Game loop till too many wrong guesses:
      while (!input.equals("!") 
            && !foundFullWord(lettersToFind)
            && wrongGuesses < maxWrongGuesses) {

         // Show prompt with score, list of guesses, blanks
         System.out.print(display(answer, lettersToFind, lettersGuessed, 
               wrongGuesses, maxWrongGuesses));

         System.out.print("\nGuess a letter (or ! to quit): ");
         input = kbScan.next();
         char guess = Character.toLowerCase(input.charAt(0));

         // Check for quit or unusable input
         if (guess == '!') {
            continue;
         } else if (!isValidInput(guess)) {
            System.out.println("Invalid input: Try again with a letter a-z");
            continue;
         }

         // Evaluate guess, Report result
         if (isCorrectGuess(guess, lettersToFind)) {
            recordFoundLetter(lettersToFind, guess);
            System.out.println("Right!");
         } else {
            ++wrongGuesses;
            System.out.println("Try again!");
         } 
         recordGuess(lettersGuessed, guess);
      }

      // Report result of game
      System.out.println(blanks(answer, lettersToFind));
      System.out.printf("\nThe word was %s.\n", answer.toUpperCase());
      
      System.out.println("Thank you for playing Hangman!");
   }

   /**
    * Is this character in the valid range ('a' through 'z')?
    *
    * @param c Character to test
    * @return True if character is valid
    */
   static boolean isValidInput(char c) {
      return c >= 'a' && c <= 'z';
   }

   /**
    * Get catalog index of a character to lookup in a boolean array created
    * with letterCatalog().
    *
    * @param c Character to lookup
    * @return Index relative to (int)'a'
    */
   static int charToIndex(char c) {
      return (int)c - (int)'a';
   }

   /**
    * Get the character reference by an index of the letter catalog.
    *
    * @param i Integer intex (offset from (int)'a')
    * @return Character in the catalog
    */
   static char charFromIndex(int i) {
      return (char)(i + (int)'a');
   }

   /**
    * Create an empty catalog of boolean values matching alphabet letters
    * a-z. Initialized by default to all false.
    *
    * @return Boolean array, length of the basic alphabet (plain ASCII)
    */
   static boolean[] letterCatalog() {
      return new boolean[(int)'z' - (int)'a' + 1];
   }

   /**
    * Create and populate a catalog of boolean values matching alphabet
    * letters matching those in the given word. All letters in that word will
    * be marked true in the catalog.
    *
    * @param word String whose letters are to be cataloged
    * @return Boolean array with positions set to true to match letters in
    *       word
    */
   static boolean[] letterCatalog(String word) {
      boolean[] catalog = letterCatalog();
      for (int i = 0; i < word.length(); ++i) {
         int index = charToIndex(word.charAt(i));
         if (index > 0 && index < catalog.length) {
            catalog[index] = true;
         }
      }
      return catalog;
   }

   /**
    * Lookup the boolean value for a single character in the letter catalog.
    *
    * @param catalog Boolean array matching alphabet letters
    * @param c Character to lookup
    * @return Boolean value for that letter
    */
   static boolean catalogValue(boolean[] catalog, char c) {
      return catalog[charToIndex(c)];
   }

   /**
    * Set the value of the letter catalog for a given letter to a given
    * value.
    *
    * @param catalog Boolean array matching alphabet letters
    * @param c Character whose value is to be set
    * @param value Value to set it to
    */
   static void setCatalogValue(boolean[] catalog, char c, 
         boolean value) {
      catalog[charToIndex(c)] = value;
   }

   /** 
    * Has the user found the full word yet? I.e., have all the letters in the
    * answer been marked false in the lettersToFind catalog?
    *
    * @param lettersToFind Boolean array with catalog of letters, 
    *           marked true if not yet found
    * @return True if there are no more characters left to find
    */
   public static boolean foundFullWord(boolean[] lettersToFind) {
      boolean anyLeftToFind = false;

      for (int i = 0; i < lettersToFind.length && !anyLeftToFind; ++i) {
         if (lettersToFind[i]) {
            anyLeftToFind = true;
         }
      }
      return !anyLeftToFind;
   }

   /** 
    * Is this a correct letter guess? 
    * Yes, if the guess is still marked as "to be found" in the letter
    * catalog. (That means if it was previously found the result will be
    * false.)
    *
    * @param guess Character guessed
    * @param lettersToFind Boolean array letter catalog
    * @return True if the letter was just found
    */
   static boolean isCorrectGuess(char guess, boolean[] lettersToFind) {
      return catalogValue(lettersToFind, guess);
   }

   /** 
    * Update the letter catalog to indicate that a new character has been
    * found.
    *
    * @param lettersToFind Boolean array of letters still to be found
    * @param letter Character that was found
    */
   static void recordFoundLetter(boolean[] lettersToFind, char letter) { 
      setCatalogValue(lettersToFind, letter, false);
   }

   /** 
    * Record the guessed character in the catalog of letters guessed.
    *
    * @param lettersGuessed Boolean array catalog of letters already guessed
    * @param guess Character guessed
    */
   static void recordGuess(boolean[] lettersGuessed, char guess) {
      setCatalogValue(lettersGuessed, guess, true);
   }
  
   /** 
    * Create a string to display the mystery word, showing letters that have
    * been guessed and blanks for the rest.
    *
    * @param word The mystery word
    * @param lettersToFind Boolean array catalog of letters still not 
    *           found in the word
    * @return String with blanks or letters found.
    */
   static String blanks(String word, boolean[] lettersToFind) {
      final char BLANK = '_';
      StringBuilder blanks = new StringBuilder();
      for (int i = 0; i < word.length(); ++i) {
         char thisChar = word.charAt(i);
         char next = catalogValue(lettersToFind, thisChar) 
                        ? BLANK : thisChar;
         blanks.append(next);
      }
      return blanks.toString();
   }

   /**
    * Create a string containing all the letters that have been guessed in
    * the catalog.
    *
    * @param catalog Boolean array matching alphabet characters
    * @return String with all letters marked as true in the catalog
    */
   static String reportGuessed(boolean[] catalog) {
      StringBuilder report = new StringBuilder();
      for (int i = 0; i < catalog.length; ++i) {
         if (catalog[i]) {
            report.append(charFromIndex(i));
         }
      }
      return report.toString();
   }

   /** 
    * Create a string with the main game display (i.e., render the current
    * game state): includes the current state of the mystery word (blanks or
    * correctly guessed letters), number of guesses remaining, and letters
    * already guessed.
    *
    * @param answer The mystery word
    * @param lettersToFind Boolean array catalog of letters in mystery word
    *           still to be found
    * @param lettersGuessed Boolean array catalog of letters already guessed
    * @return String displaying game state
    */
   static String display(String answer, boolean[] lettersToFind,
         boolean[] lettersGuessed,
         int wrongGuesses, int maxWrongGuesses) {

      String blanks = blanks(answer, lettersToFind);
      String guesses = String.format("Letters guessed: %s", 
            reportGuessed(lettersGuessed));
      String score = String.format("Guesses remaining: %d", 
            maxWrongGuesses - wrongGuesses);

      return String.format("\n%-20s\n%-20s\n%-20s\n", blanks, guesses, score);
   }
}
