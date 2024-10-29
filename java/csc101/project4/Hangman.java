// TODO
// - way to evaluate if the full word has been guessed
// - (sort letters guessed in display)
// - (repeated letter is wrong even if the letter is right)
// - (show display once more when complete)
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

         // Show prompt
         System.out.print(display(answer, found, lettersGuessed, 
               wrongGuesses, maxWrongGuesses));

         System.out.print("\nGuess a letter (or ! to quit): ");
         input = kbScan.next();
         char guess = input.charAt(0);
         if (guess == '!') {
            continue;
         }

         // Evaluate guess
         lettersGuessed = recordGuess(lettersGuessed, guess);
         if (!isCorrectGuess(guess, answer)) {
            ++wrongGuesses;
            System.out.println("Wrong!");
         } else {
            recordFoundLetter(found, guess, answer);
            System.out.println("Right!");
         }

         // Report result of guess
         // Revise prompt, update score, list of guesses
      }

      System.out.println(blanks(answer, found));
      System.out.printf("\nThe word was %s.\n", answer.toUpperCase());

      // Report result of game
      System.out.println("Thank you for playing Hangman!");
   }

   static String recordGuess(String lettersGuessed, char guess) {
      if (!contains(lettersGuessed, guess)) {
         String addition = Character.toString(guess);

         if (lettersGuessed.length() < 1 ||
               guess > lettersGuessed.charAt(lettersGuessed.length() - 1)) {
            lettersGuessed += addition;
         } else {
            lettersGuessed = addition + lettersGuessed;
         }
      }
      return lettersGuessed;
   }
   
   static void recordFoundLetter(boolean[] found, char letter, 
         String answer) { 
      for (int i = 0; i < found.length; ++i) {
         if (answer.charAt(i) == letter) {
            found[i] = true;
         }
      }
   }

   static boolean contains(String str, char guess) {
      return str.indexOf(guess) >= 0;
   }

   static boolean isCorrectGuess(char guess, String answer) {
      return contains(answer, guess);
   }

   static String blanks(String word, boolean[] found) {
      final char BLANK = '_';
      StringBuilder blanks = new StringBuilder();
      for (int i = 0; i < found.length; ++i) {
         char next = found[i] ? word.charAt(i) : BLANK;
         blanks.append(next);
      }
      return blanks.toString();
   }

   public static String display(String answer, boolean[] found,
         String lettersGuessed,
         int wrongGuesses, int maxWrongGuesses) {

      String blanks = blanks(answer, found);
      String guesses = String.format("Letters guessed: %s", 
            lettersGuessed.toString());
      String score = String.format("Guesses remaining: %d", 
            maxWrongGuesses - wrongGuesses);

      return String.format("\n%-20s\n%-20s\n%-20s\n", blanks, guesses, score);
   }

   static boolean every(boolean[] ls) {
      boolean result = true;
      for (int i = 0; i < ls.length && result; ++i) {
         result = ls[i];
      }
      return result;
   }

   public static boolean isGuessed(String answer, String lettersGuessed) {
      boolean[] letters = new boolean[answer.length()];

      for (int i = 0; i < answer.length(); ++i) {
         letters[i] = contains(lettersGuessed, answer.charAt(i));
      }
      return every(letters);
   }

   
}
