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
/** 
 * A catalog of boolean values matching alphabet letters a-z. 
 */
class LetterCatalog {
   private boolean[] catalog;

   /** Create an empty catalog of boolean values matching alphabet letters
    * a-z. Initialized by default to all false.
    */
   public LetterCatalog() {
      this.catalog = new boolean[(int)'z' - (int)'a' + 1];
   }
   
   /**
    * Create and populate a catalog of boolean values matching alphabet
    * letters matching those in the given word. All letters in that word
    * will be marked true in the catalog.
    *
    * @param word String whose letters are to be cataloged
    */
   public LetterCatalog(String word) {
      this();
      for (int i = 0; i < word.length(); ++i) {
         int index = charToIndex(word.charAt(i));
         if (index > 0 && index < catalog.length) {
            catalog[index] = true;
         }
      }
   }

   /**
    * Look up the boolean value for a single character in the letter
    * catalog.
    *
    * @param c Character to lookup
    * @return Boolean value for that letter
    */
   public boolean getValue(char c) {
      return this.catalog[charToIndex(c)];
   }

   /**
    * Set the value of the letter catalog for a given letter to a given
    * value.
    *
    * @param c Character whose value is to be set
    * @param value Value to set it to
    */
   public void setValue(char c, boolean value) {
      this.catalog[charToIndex(c)] = value;
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
    * Are any of the values in the catalog true?
    *
    * @return True if any are true
    */
   public boolean any() {
      boolean anyTrue = false;
      for (boolean value: this.catalog) {
         if (value) {
            anyTrue = true;
         }
      }
      return anyTrue;
   }
   
   /**
    * Create a string containing all the letters that have been guessed in
    * the catalog.
    *
    * @return String with all letters marked as true in the catalog (sorted
    * alphabetically by default)
    */
   public String toString() {
      StringBuilder report = new StringBuilder();
      for (int i = 0; i < catalog.length; ++i) {
         if (catalog[i]) {
            report.append(charFromIndex(i));
         }
      }
      return report.toString();
   }
}
/**
 * The main state of our game, tracking the current number of guesses,
 * letters to be found, and letters guessed; able to report whether the
 * game is finished, and to return strings for displaying the game state.
 */
class GameState {
   private String answer;
   private int maxWrongGuesses;
   private int wrongGuesses;
   private LetterCatalog lettersToFind;
   private LetterCatalog lettersGuessed;

   /**
    * Initialize game state given the user's mystery word and maximum number
    * of wrong guesses. Set up letter catalogs for letters in the mystery
    * word to find (true means a letter remains to be found) and letters
    * that have been guessed (true means a letter has been guessed).
    *
    * @param answer String with the user's mystery word
    * @param maxWrongGuesses Integer value of maximum wrong guesses
    */
   public GameState(String answer, int maxWrongGuesses) {
      this.answer = answer.toLowerCase();
      this.maxWrongGuesses = maxWrongGuesses;
      this.wrongGuesses = 0;
      this.lettersToFind = new LetterCatalog(this.answer);
      this.lettersGuessed = new LetterCatalog();
   }

   public void incWrongGuesses() {
      ++this.wrongGuesses;
   }

   /**
    * Is the game finished? I.e., no more letters left to find or no guesses
    * remaining.
    *
    * @return True if game is finished
    */
   public boolean isFinished() {
      return !this.lettersToFind.any() || this.guessesRemaining() <= 0;
   }

   /** 
    * How many wrong guesses can still be made?
    *
    * @return Number of guesses left before reaching max wrong guesses.
    */
   private int guessesRemaining() {
      return this.maxWrongGuesses - this.wrongGuesses;
   }

   /** 
    * Is this a correct letter guess? 
    * Yes, if the guess is still marked as "to be found" in the letter
    * catalog. (That means if it was previously found the result will be
    * false.)
    *
    * @param guess Character guessed
    * @return True if the letter was just found
    */
   public boolean isCorrectGuess(char guess) {
      return this.lettersToFind.getValue(guess);
   }

   /** 
    * Update the letter catalog to indicate that a new character has been
    * found.
    *
    * @param letter Character that was found
    */
   public void recordFoundLetter(char letter) { 
      this.lettersToFind.setValue(letter, false);
   }
   
   /** 
    * Record the guessed character in the catalog of letters guessed.
    *
    * @param guess Character guessed
    */
   public void recordGuess(char guess) {
      lettersGuessed.setValue(guess, true);
   }

   /** 
    * Create a string to display the mystery word, showing letters that have
    * been guessed and blanks for the rest.
    *
    * @return String with blanks or letters found.
    */
   public String blanks() {
      String word = this.answer;
      final char BLANK = '_';
      StringBuilder blanks = new StringBuilder();
      for (int i = 0; i < word.length(); ++i) {
         char thisChar = word.charAt(i);
         char next = this.lettersToFind.getValue(thisChar) 
             ? BLANK : thisChar;
         blanks.append(next);
      }
      return blanks.toString();
   }

   /** 
    * Create a string with the main game display (i.e., render the current
    * game state): includes the current state of the mystery word (blanks or
    * correctly guessed letters), number of guesses remaining, and letters
    * already guessed.
    
    * @return String displaying game state
    */
   public String display() {
      String blanks = this.blanks();
      String guesses = String.format("Letters guessed: %s", 
            this.lettersGuessed);
      String score = String.format("Guesses remaining: %d", 
            this.guessesRemaining());

      return String.format("\n%-20s\n%-20s\n%-20s\n", 
            blanks, guesses, score);
   }

   /**
    * Show the final answer.
    *
    * @return String with message revealing the mystery word.
    */
   public String revealAnswer() {
      return String.format("The word was %s.", this.answer.toUpperCase());
   }
}

