namespace Musarithmetic;

public class Interval
{
    Quality quality;
    int degree;

    public int Degree { get => degree; }

    public Interval(Quality quality, int degree)
    {
        if (quality.MatchesDegree(degree))
        {
            this.quality = quality;
            this.degree = degree;
        } else
            throw new ArgumentException($"Invalid quality/degree combination {quality}/{degree + 1}");
    }

    public Interval(Pitch p1, Pitch p2)
    {
        degree = DiatonicInterval(p1, p2);

        int defaultInterval = new PitchName(degree).ChromaticValue;
        int inflectedInterval = ChromaticInterval(p1, p2);
        int adjustment = inflectedInterval - defaultInterval;

        try 
        {
            quality = new Quality(degree, adjustment);
        }
        catch { throw; }
    }

    public static Interval FromString(string input)
    {
        Interval interval;

        void ThrowInputException() => 
            throw new Exception($"Could not create Interval from string {input}");

        if (input.Length >= 2)
        {
            Quality quality;

            try
            {
                quality = new Quality(input[..1]); 
            }
            catch { throw; }
    
            int degree;
            bool test = int.TryParse(input[1..], out degree);
            if (test)
            {
                Interval i = new(quality, degree - 1);
                return i;
            }
            else 
                ThrowInputException();
        }
        else
            ThrowInputException();

        return interval;
    }

    public static int LoopDiff(int n, int m, int max) => 
        (n < m) ? n - m + max : n - m;

    int DiatonicInterval(Pitch p1, Pitch p2) =>
        LoopDiff(p1.DiatonicValue, p2.DiatonicValue, 7);

    int ChromaticInterval(Pitch p1, Pitch p2) =>
        LoopDiff(p1.ChromaticValue, p2.ChromaticValue, 12);

    public override string ToString() => $"{quality}{degree + 1}";

    public int ChromaticValue { 
        get 
        {
            int offset = PitchName.ChromaticOffset(degree);
            int chromaticValue = offset + quality.Adjustment(degree);
            if (degree < 0) chromaticValue *= -1;

            return chromaticValue;
        }
    }

    public Interval Negate() => new Interval(this.quality, -this.degree);

}

