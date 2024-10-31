import java.util.Random;

/**
 * Frequency of integers in a random sample
 *
 * @author Andrew Cashner, <code>acashner@student.monroecc.edu</code>
 * @version 2024/10/25 (CSC 101, Project 4)
 */
public class RandomFrequency {
    /** 
     * Generate 1000 random integers n, 1 &lt;= n &lt;= 50, and store in
     * array.  Report the 10 most frequently occuring numbers with their
     * frequencies.  NB: Use 1 as seed for random-number generator for
     * classroom purposes.
     *
     * @param (Unused) Command-line arguments
     */
    public static void main(String[] args) {
        final int MIN = 1;
        final int MAX = 50;
        final int RANDOM_COUNT = 1000;
        final int FREQUENCY_COUNT = 10;

//        Random randomizer = new Random(1);
        Random randomizer = new Random();

        RandomCollection randoms = new RandomCollection(randomizer, 
                RANDOM_COUNT, MIN, MAX);
        FrequencyList topFrequencies = randoms.mostFrequent(FREQUENCY_COUNT);
        System.out.print(topFrequencies);
    }

}

class RandomCollection {
    private Random randomizer;
    private int size;
    private int min;
    private int max;
    private int[] randoms;
    private int[] frequency;

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

    private int randomInt() {
        return this.randomizer.nextInt(this.max) + this.min;
    }

    private Frequency maxFrequency(Frequency ceiling) {
        int maxValue = 0;
        int maxPosition = 0;

        for (int i = 0; i < this.frequency.length; ++i) {
            if (this.frequency[i] > maxValue 
                    && ceiling.frequencyGreaterThan(this.frequency[i])) {

                maxValue = this.frequency[i];
                maxPosition = i;
            }
        }
        return new Frequency(maxPosition + this.min, maxValue);
    }

    public FrequencyList mostFrequent(int quantity) {
        FrequencyList sorted = new FrequencyList(quantity);

        Frequency previousMax = new Frequency(0, this.size);
        for (int i = 0; i < quantity; ++i) {
            Frequency currentMax = this.maxFrequency(previousMax);
            sorted.setValue(i, currentMax);
            previousMax = currentMax;
        }
        return sorted;
    }
}

class Frequency {
    private int number;
    private int frequency;
    public static int formatColumnWidth = 10;

    public Frequency(int number, int frequency) {
        this.number = number;
        this.frequency = frequency;
    }

    public int indexFromNumber(int min) {
        return this.number - min;
    }
    
    public boolean frequencyGreaterThan(int comparison) {
        return this.frequency > comparison;
    }

    public String toString() {
        String rowFormat = "%" + formatColumnWidth + "s %" + formatColumnWidth + "s";
        return String.format(rowFormat, this.number, this.frequency);
    }
}

class FrequencyList {
    private Frequency[] list;

    public FrequencyList(int quantity) {
        this.list = new Frequency[quantity];
    }

    public FrequencyList(Frequency[] list) {
        this(list.length);
        for (int i = 0; i < list.length; ++i) {
            this.list[i] = list[i];
        }
    }

    public void setValue(int index, Frequency value) {
        this.list[index] = value;
    }

    public String toString() {
        StringBuilder report = new StringBuilder();
        String rowFormat = "%" + Frequency.formatColumnWidth + "s %" + Frequency.formatColumnWidth + "s\n";
        report.append(String.format(rowFormat, "Number", "Frequency"));
        for (Frequency freq: this.list) {
            report.append(String.format("%s\n", freq));
        }
        return report.toString();
    }
}

