using System.Text.RegularExpressions;

namespace Harmonics;

public class HarmonicSeries
{
    public int[] Partials { get; private set; }

    public HarmonicSeries(int fundamental, int numPartials)
    {
        if (fundamental < 0)
        {
            throw new ArgumentException("Fundamental must be positive");
        }

        if (numPartials < 0)
        {
            throw new ArgumentException("Number of partials must be positive");
        }

        Partials = new int[numPartials];

        int current = fundamental;
        Partials[0] = current;

        for (int i = 1; i < numPartials; ++i)
        {
            current += fundamental;
            Partials[i] = current;
        }
    }

    public int this[int i] => Partials[i];

    public int AtRatio(string ratio)
    {
        Match match = Regex.Match(ratio, @"([0-9].*):([0-9].*)");

        if (!match.Success)
        {
            throw new ArgumentException($"Invalid ratio string '{ratio}'");
        }

        string numeratorString = match.Groups[1].Value;

        try 
        {
            int numerator = Int32.Parse(numeratorString);

            if (numerator > Partials.Length || numerator < 1)
            {
                throw new ArgumentException($"Requested partial at ratio '{ratio}' is out of range");
            }
            
            return Partials[numerator - 1];
        }
        catch (System.FormatException ex)
        {
            throw new ArgumentException($"Invalid numeric format: {ex.Message}");
        }

    }

    public int Length => Partials.Length;
}