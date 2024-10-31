import java.util.Scanner;

/**
 * Hangman Game (without the hangman!)
 *
 * Given a mystery word and a maximum number of wrong guesses, run a word
 * guessing game where the user guesses one letter at a time.
 *
 * We implement this using two arrays of boolean values matching the
 * alphabet letters a through z. The first letter catalog records letters
 * in the mystery word that still remain to be found; as each letter is
 * found, those entries are marked false. The second letter catalog records
 * the letters that have already been guessed; as each letter is guessed,
 * that entry is marked true.  The user wins when there are no more true
 * values in the lettersToFind catalog, because all have been found.
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

      System.out.print("Enter the maximum number of wrong guesses: ");
      int maxWrongGuesses = kbScan.nextInt();

      // Create game from user values
      GameState gameState = new GameState(answer, maxWrongGuesses);


      // Game loop till too many wrong guesses or user quits
      String input = new String();
      while (!input.equals("!") && !gameState.isFinished()) {

         // Show prompt with score, list of guesses, blanks
         System.out.print(gameState.display());

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
         if (gameState.isCorrectGuess(guess)) {
            gameState.recordFoundLetter(guess);
            System.out.println("Right!");
         } else {
            gameState.incWrongGuesses();
            System.out.println("Try again!");
         } 
         gameState.recordGuess(guess);
      }

      // Report result of game
      System.out.println(gameState.blanks());
      System.out.printf("\n%s\n", gameState.revealAnswer());

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
}
