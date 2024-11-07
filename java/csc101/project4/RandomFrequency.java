import java.util.Random;

/**
 * Report the frequency of integers in a random sample.
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/25 (CSC 101, Project 4)
 */
public class RandomFrequency {
   /** 
    * Generate 1000 random integers n, 1 &lt;= n &lt;= 50, and store in
    * array. Report the 10 most frequently occuring numbers with their
    * frequencies. 
    *
    * NB: Use 1 as seed for random-number generator for classroom purposes.
    *
    * @param args (Unused) Command-line arguments
    */
   public static void main(String[] args) {
      final int MIN = 1;
      final int MAX = 50;
      final int RANDOM_COUNT = 1000;
      final int FREQUENCY_COUNT = 10;

      Random randomizer = new Random(1);

      RandomCollection randoms = new RandomCollection(randomizer, 
            RANDOM_COUNT, MIN, MAX);
      FrequencyList topFrequencies = randoms.mostFrequent(FREQUENCY_COUNT);
      System.out.print(topFrequencies);
   }

}

/**
 * A collection of random numbers.
 */
class RandomCollection {
   /** Random number generator, already seeded */
   private Random randomizer;

   /** Quantity of random numbers in the collection */
   private int size;

   /** Minimum value of random numbers */
   private int min;

   /** Maximum value of random numbers */
   private int max;

   /** Array of random numbers */
   private int[] randoms;

   /** 
    * Array tracking the frequency of each random number in this.randoms.
    * The index is the number and the value at that index is its frequency.
    */
   private int[] frequency;

   /**
    * Create and populate a new collection of random numbers of given size,
    * ranging from min to max, and tabulate the frequency of each number as
    * it is generated.
    *
    * @param randomizer Random number generator
    * @param size Quantity of random numbers to be generated
    * @param min Minimum value
    * @param max maximum value
    */
   public RandomCollection(Random randomizer, int size, int min, int max) {
      this.randomizer = randomizer;
      this.size = size;
      this.min = min;
      this.max = max;
      this.randoms = new int[size];
      this.frequency = new int[size];

      for (int i = 0; i < size; ++i) {
         int newRandom = this.randomInt();
         this.randoms[i] = newRandom;
         ++this.frequency[newRandom - this.min];
      }
   }

   /**
    * Generate a new random number
    *
    * @return Random integer between already given max and min
    */
   private int randomInt() {
      return this.randomizer.nextInt(this.max) + this.min;
   }

   /**
    * Return a Frequency object holding the most-frequent number in this
    * random collection and its frequency.
    *
    * @param ceiling Frequency object of the previous maximum found (so we
    *      can make a ranked list of the most-occurring numbers)
    * @return Frequency object with the most-frequent number and its
    *      frequency
    */
   private Frequency maxFrequency(Frequency ceiling) {
      int maxValue = 0;
      int maxPosition = 0;

      for (int i = 0; i < this.size; ++i) {
         if (this.frequency[i] > maxValue 
               && ceiling.frequencyGreaterThan(this.frequency[i])) {

            maxValue = this.frequency[i];
            maxPosition = i;
        }
      }
      return new Frequency(maxPosition + this.min, maxValue);
   }

   /**
    * Return a list of the most frequent n random numbers for a given n.
    *
    * @param quantity Integer number n for top-n most frequent numbers
    * @return FrequencyList with the n most frequent numbers and their
    *  frequencies
    */
   public FrequencyList mostFrequent(int quantity) {
      FrequencyList sorted = new FrequencyList(quantity);

      // The highest maximum frequency would be equal to the size of the
      // array
      Frequency previousMax = new Frequency(0, this.size);

      for (int i = 0; i < quantity; ++i) {
         Frequency currentMax = this.maxFrequency(previousMax);
         sorted.setValue(i, currentMax);
         previousMax = currentMax;
      }
      return sorted;
   }
}

/**
 * Records a number and its frequency of occurrence in the random collection.
 */
class Frequency {
   /** The number */
   private int number;

   /** How many times it appears in a random collection */
   private int frequency;

   /**
    * Create a new Frequency given a number and its frequency.
    *
    * @param number The number
    * @param frequency Its frequency
    */
   public Frequency(int number, int frequency) {
      this.number = number;
      this.frequency = frequency;
   }

   /** 
    * Make a copy of this object. 
    *
    * @return New Frequency with same data as this
    */
   public Frequency clone() {
      return new Frequency(this.number, this.frequency);
   }

   /**
    * Get the index for RandomCollection.frequency for a given number.
    * The RandomCollection frequency array is zero-indexed and gives numbers
    * relative to the minimum value used to create the collection.
    *
    * @param min The minimum value used to create the collection
    * @return Integer index used to represent that number in
    *      RandomCollection.frequency
    */
   public int indexFromNumber(int min) {
      return this.number - min;
   }

   /**
    * Is this frequency more than the given number?
    *
    * @param comparison Number to compare
    * @return True if this is greater
    */
   public boolean frequencyGreaterThan(int comparison) {
      return this.frequency > comparison;
   }

   /** Width of columns in tabular output */
   private static final int FORMAT_COLUMN_WIDTH = 10;

   /** 
    * Allows consistent formatting of table output for individual Frequency
    * objects and lists of them
    *
    * @param conversionSymbol Symbol used in String.format (e.g., 'd' or 's')
    * @return String used as first argument to String.format
    */
   public static String formatSpecifier(char conversionSymbol) {
      String oneColumn = String.format("%%%d%c", FORMAT_COLUMN_WIDTH, 
            conversionSymbol);
      return String.format("%s %s", oneColumn, oneColumn);
   }

   /** Format string for individual Frequency */
   private static String format = Frequency.formatSpecifier('d');

   /** 
    * Tabular string output of this Frequency data 
    *
    * @return String in tabular format
    */
   public String toString() {
      return String.format(this.format, this.number, this.frequency);
   }
}

/**
 * A list of Frequency objects
 */
class FrequencyList {
   /** An array of Frequency objects */
   private Frequency[] list;

   /** Create a new array of a given quantity of Frequency objects */
   public FrequencyList(int quantity) {
      this.list = new Frequency[quantity];
   }

   /** Copy a given Frequency array into this object (deep copy) */
   public FrequencyList(Frequency[] list) {
      this(list.length);
      for (int i = 0; i < list.length; ++i) {
         this.list[i] = list[i].clone();
      }
   }

   /** 
    * Set the value of this array at a given index to a given value.
    *
    * @param index Integer index of the array
    * @param value Frequency to copy there
    */
   public void setValue(int index, Frequency value) {
      this.list[index] = value.clone();
   }

   /** Tabular string format for this list */
   private static String format = Frequency.formatSpecifier('s') + "\n";

   /**
    * Print the contents of this list as a two-column table of numbers and
    * their frequencies.
    *
    * @return Table as string
    */
   public String toString() {
      StringBuilder report = new StringBuilder();
      report.append(String.format(this.format, "Number", "Frequency"));
      for (Frequency freq: this.list) {
         report.append(String.format("%s\n", freq));
      }
      return report.toString();
   }
}

