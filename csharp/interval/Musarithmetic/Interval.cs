namespace Musarithmetic;

public class Interval
{
    Quality quality;
    int degree;

    public int Degree { get => degree; }

    public Interval(Quality quality, int degree)
    {
        bool IsValid(Quality quality, int degree) =>
            (quality.IsPerfect() 
             && QualityHelper.IsPerfectDegree(degree))
                || (quality.IsImperfect() 
                        && !QualityHelper.IsPerfectDegree(degree));

        if (IsValid(quality, Math.Abs(degree)))
        {
            this.quality = quality;
            this.degree = degree;
        } else
            throw new ArgumentException($"Invalid quality/degree combination {quality.ToSymbol()}/{degree + 1}");
    }

    public Interval(Pitch p1, Pitch p2)
    {
        degree = DiatonicInterval(p1, p2);

        int defaultInterval = PitchNameHelper.ChromaticOffset(degree);
        int inflectedInterval = ChromaticInterval(p1, p2);
        int adjustment = inflectedInterval - defaultInterval;

        try 
        {
            quality = QualityHelper.FromSize(degree, adjustment);
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
                quality = QualityHelper.FromString(input[..1]); 
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

    // TODO replace this and next with generics?
    static int DiatonicInterval(Pitch p1, Pitch p2)
    {
        int val1 = p1.DiatonicValue();
        int val2 = p2.DiatonicValue();
        return LoopDiff(val1, val2, 7);
    }

    static int ChromaticInterval(Pitch p1, Pitch p2)
    {
        int val1 = p1.ChromaticValue();
        int val2 = p2.ChromaticValue();
        return LoopDiff(val1, val2, 12);
    }


    public override string ToString()
    {
        return quality.ToSymbol() + $"{degree + 1}";
    }

    public int ChromaticValue()
    {
        int offset = PitchNameHelper.ChromaticOffset(degree % 7);
        int adjustment = quality switch
        {
            Quality.DIMINISHED => QualityHelper.IsPerfectDegree(degree) ? -1 : -2,
            Quality.MINOR => -1,
            Quality.MAJOR or Quality.PERFECT => 0,
            Quality.AUGMENTED => 1,
            _ => throw new ArgumentException($"Cannot calculate chromatic value of interval quality {quality}")
        };

        int chromaticValue = offset + adjustment;
        if (degree < 0) chromaticValue *= -1;

        return chromaticValue;
    }

    public Interval Negate()
    {
        return new Interval(this.quality, -this.degree);
    }

}

