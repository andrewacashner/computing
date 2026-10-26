namespace Harmonics;

public class HarmonicSeries
{
    public static int[] Generate(int fundamental, int numPartials)
    {
        if (fundamental < 0)
        {
            throw new ArgumentException("Fundamental must be positive");
        }

        int[] partials = new int[numPartials];

        int current = fundamental;
        partials[0] = current;

        for (int i = 1; i < numPartials; ++i)
        {
            current += fundamental;
            partials[i] = current;
        }

       return partials; 
    }
}