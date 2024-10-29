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

      // Game loop till too many wrong guesses:
      while (!input.equals("!") 
            && !isGuessed(answer, lettersGuessed)
            && wrongGuesses < maxWrongGuesses) {

         // Show prompt
         System.out.print(display(answer, lettersGuessed, 
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
            System.out.println("Right!");
         }

         // Report result of guess
         // Revise prompt, update score, list of guesses
      }

      System.out.println(blanks(answer, lettersGuessed));
      System.out.printf("\nThe word was %s.\n", answer.toUpperCase());

      // Report result of game
      System.out.println("Thank you for playing Hangman!");
   }

   static String recordGuess(String lettersGuessed, char guess) {
      if (!contains(lettersGuessed, guess)) {
         lettersGuessed += Character.toString(guess);
      }
      return lettersGuessed;
   }

   static boolean contains(String str, char guess) {
      return str.indexOf(guess) >= 0;
   }

   static boolean isCorrectGuess(char guess, String answer) {
      return contains(answer, guess);
   }

   static char matchOrBlank(String word, String lettersToShow, int i) {
      final char BLANK = '_';
      
      char thisChar = word.charAt(i);
      char displayChar = BLANK;
  
      if (lettersToShow.contains(Character.toString(thisChar))) {
         displayChar = thisChar;
      } 

      return displayChar;
   }

   static String blanks(String word, String lettersToShow) {
      StringBuilder blanks = new StringBuilder();

      for (int i = 0; i < word.length(); ++i) {
         char newChar = matchOrBlank(word, lettersToShow, i);
         blanks.append(String.format("%c ", newChar));
      }
      return blanks.toString();
   }

   public static String display(String answer, String lettersGuessed,
         int wrongGuesses, int maxWrongGuesses) {

      String blanks = blanks(answer, lettersGuessed);
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
