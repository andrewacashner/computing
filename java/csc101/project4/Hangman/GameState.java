/**
 * The main state of our game, tracking the current number of guesses,
 * letters to be found, and letters guessed; able to report whether the
 * game is finished, and to return strings for displaying the game state.
 */
public class GameState {
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

