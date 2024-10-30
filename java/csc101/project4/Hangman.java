// TODO
// - way to evaluate if the full word has been guessed
// - (BONUS):
//    - (display a hangman!)
//    - (get answer from random entry in (online) dictionary
//

import java.util.Scanner;

/**
 * Hangman Game
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/25 (CSC 101, Project 4)
 */
public class Hangman {
   /** 
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
      String lettersGuessed = new String();

      String input = new String();
      boolean[] found = new boolean[answer.length()];

      // Game loop till too many wrong guesses:
      while (!input.equals("!") 
            && !isGuessed(answer, lettersGuessed)
            && wrongGuesses < maxWrongGuesses) {

         // Show prompt with score, list of guesses, blanks
         System.out.print(display(answer, found, lettersGuessed, 
               wrongGuesses, maxWrongGuesses));

         System.out.print("\nGuess a letter (or ! to quit): ");
         input = kbScan.next();
         char guess = Character.toLowerCase(firstChar(input));
         if (guess == '!') {
            continue;
         }

         // Evaluate guess, Report result
         if (!isCorrectGuess(guess, answer, lettersGuessed)) {
            ++wrongGuesses;
            System.out.println("Try again!");
         } else {
            found = recordFoundLetter(found, guess, answer);
            System.out.println("Right!");
         }
         lettersGuessed = recordGuess(lettersGuessed, guess);
      }

      System.out.println(blanks(answer, found));
      System.out.printf("\nThe word was %s.\n", answer.toUpperCase());

      // Report result of game
      System.out.println("Thank you for playing Hangman!");
   }

   /**
    * Return the first character of a string, or 0 if the string is empty.
    *
    * @param str The string
    * @return Its first character
    */
   static char firstChar(String str) {
      char first = 0;
      if (str.length() > 0) {
         first = str.charAt(0);
      } 
      return first;
   }

   /** Return the last character of a string, or 0 if the string is empty.
    *
    * @param str The string
    * @return Its last character
    */
   static char lastChar(String str) {
      char last = 0;
      if (str.length() > 0) {
         last = str.charAt(str.length() - 1);
      } 
      return last;
   }

   /**
    * Is every element in the array true?
    *
    * @param ls Array of booleans
    * @return True if all are true; false if any are false
    */
   static boolean every(boolean[] ls) {
      boolean result = true;
      for (int i = 0; i < ls.length && result; ++i) {
         result = ls[i];
      }
      return result;
   }

   /**
    * Has the user found the full word yet? I.e., are all the letters in
    * 'answer' now in 'lettersGuessed'?
    *
    * @param answer String the user is trying to guess
    * @param lettersGuessed String containing characters the user has already
    *               guessed
    * @return True if all the characters in answer have been guessed
    */
   public static boolean isGuessed(String answer, String lettersGuessed) {
      boolean[] letters = new boolean[answer.length()];

      for (int i = 0; i < answer.length(); ++i) {
         letters[i] = contains(lettersGuessed, answer.charAt(i));
      }
      return every(letters);
   }

   /**
    * Does this string contain the given char? (DIY version of existing
    * library function)
    *
    * @param str String to search
    * @param match Character to search for
    * @return True if string contains the character
    */
   static boolean contains(String str, char match) {
      return str.indexOf(match) >= 0;
   }
  
   /**
    * Is this a correct letter guess? It is correct if the character guessed
    * is in the answer word and has not already been guessed.
    *
    * @param guess Character guessed
    * @param answer String of answer word
    * @param lettersGuessed String with letters already guessed
    * @return True if it meets the criteria above
    */
   static boolean isCorrectGuess(char guess, String answer, 
         String lettersGuessed) {
      return contains(answer, guess) && !contains(lettersGuessed, guess);
   }

   /**
    * Return an updated array of booleans indicating which letters have been
    * found in the answer word.
    *
    * @param found Current array of booleans
    * @param letter Character that was found
    * @param answer String of the answer word
    * @return New array of booleans with updated content
    */
   static boolean[] recordFoundLetter(boolean[] found, char letter, 
         String answer) { 
      boolean[] updated = new boolean[found.length];

      for (int i = 0; i < found.length; ++i) {
         updated[i] = answer.charAt(i) == letter ? true : found[i];
      }
      return updated;
   }

   /**
    * Insert a character into a string (StringBuilder) in sorted order.
    * We are assuming the string is already sorted.
    *
    * @param str Sorted string
    * @param newChar Character to insert
    * @return StringBuilder with character inserted
    */
   static StringBuilder insertSorted(String str, char newChar) {
      StringBuilder updated = new StringBuilder();

      boolean found = false;
      for (int i = 0; i < str.length(); ++i) {
         char thisChar = str.charAt(i);

         // Is this the appropriate place to insert?
         // If so, insert and then copy the current character;
         // Otherwise just copy.
         // Once inserted, don't keep testing for insertion.
         if (newChar < thisChar && !found) {
            updated.append(newChar);
            found = true;
         } 
         updated.append(thisChar);
      } 
      // If we didn't find a place to insert, the new char must go at the
      // end.
      if (!found) {
         updated.append(newChar);
      }
      return updated;
   }

   /**
    * Add the guessed character to the list of letters guessed, skipping
    * duplicates and keeping the list sorted alphabetically.
    *
    * @param lettersGuessed String with characters guessed
    * @param guess Character guessed
    * @return New string with updated letters guessed
    */
   static String recordGuess(String lettersGuessed, char guess) {
      StringBuilder updated = new StringBuilder();

      // Ignore duplicates
      if (contains(lettersGuessed, guess)) {
         updated.append(lettersGuessed);

      } else if (lettersGuessed.length() < 1) {
         updated.append(guess);

      } else if (guess < firstChar(lettersGuessed)) {
         updated.append(guess);
         updated.append(lettersGuessed);

      } else if (guess > lastChar(lettersGuessed)) {
         updated.append(lettersGuessed);
         updated.append(guess);

      } else {
         updated = insertSorted(lettersGuessed, guess);
      }

      return updated.toString();
   }
  
   /**
    * Create a string to display the mystery word, showing letters that have
    * been guessed and blanks for the rest.
    *
    * @param word The mystery word
    * @param found Array of booleans matching the characters of 'word'
    * @return String with blanks or letters found.
    */
   static String blanks(String word, boolean[] found) {
      final char BLANK = '_';
      StringBuilder blanks = new StringBuilder();
      for (int i = 0; i < found.length; ++i) {
         char next = found[i] ? word.charAt(i) : BLANK;
         blanks.append(next);
      }
      return blanks.toString();
   }

   /**
    * Create a string with the main game display (i.e., render the current
    * game state): includes the current state of the mystery word (blanks or
    * correctly guessed letters), number of guesses remaining, and letters
    * already guessed.
    *
    * @param answer The mystery word
    * @param found Array of booleans matching the characters of the mystery
    *               word, indicating which have been guessed
    * @param lettersGuessed String containing letters already guessed
    * @return String displaying game state
    */
   public static String display(String answer, boolean[] found,
         String lettersGuessed,
         int wrongGuesses, int maxWrongGuesses) {

      String blanks = blanks(answer, found);
      String guesses = String.format("Letters guessed: %s", lettersGuessed);
      String score = String.format("Guesses remaining: %d", 
            maxWrongGuesses - wrongGuesses);

      return String.format("\n%-20s\n%-20s\n%-20s\n", blanks, guesses, score);
   }
}
