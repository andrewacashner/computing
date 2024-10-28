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
      String hangman = new String();

      // Game loop till too many wrong guesses:
      while (!input.equals("!") && wrongGuesses < maxWrongGuesses) {
         // Show prompt
         hangman = drawHangman(hangman, wrongGuesses);
         String display = display(hangman, answer, lettersGuessed, 
               wrongGuesses, maxWrongGuesses);
         System.out.print(display);
         System.out.print("Guess a letter (or ! to quit): ");
         input = kbScan.next();
         char guess = input.charAt(0);
         if (guess == '!') {
            continue;
         }

         // Evaluate guess
         lettersGuessed = recordGuess(lettersGuessed, guess);
         if (!isCorrectGuess(guess, answer)) {
            ++wrongGuesses;
         }

         // Report result of guess
         // Revise prompt, update score, list of guesses

      }

      // Report result of game
      System.out.println("Thank you for playing Hangman!");
   }

   static String recordGuess(String lettersGuessed, char guess) {
      return lettersGuessed + Character.toString(guess);
   }

   static boolean contains(String str, char guess) {
      return str.indexOf(guess) > 0;
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

//        😵
//      💪👔🤳
//        👖
//       👟👟
//
   static String drawHangman(String currentHangman, int wrongGuesses) {
      final String GALLOWS = "-------¬\n";
      final String[] HANGMAN = {
         "|      😵\n",
         "|    💪", 
         "👔",
         "🤳\n",
         "|      👖\n",
         "|     👟",
         "👟\n"
      };

      String nextPart = new String();

      if (wrongGuesses < HANGMAN.length) {
         nextPart = HANGMAN[wrongGuesses];
      } 
         // nextPart = wrongGuesses % 2 > 0 ? "|  |" : "  |\n";
      return currentHangman + nextPart;
   }
     

   public static String display(String hangman, 
         String answer, String lettersGuessed,
         int wrongGuesses, int maxWrongGuesses) {
      String blanks = blanks(answer, lettersGuessed);
      String guesses = String.format("Letters guessed: %s\n", lettersGuessed.toString());
      return String.format("%-20s%s\n%s\n", blanks, guesses, hangman);
   }

}
