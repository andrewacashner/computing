/** 
 * A catalog of boolean values matching alphabet letters a-z. 
 */
public class LetterCatalog {
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
