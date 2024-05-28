namespace Musarithmetic;

public class Interval
{
    Quality quality;
    int degree;

    public Interval(Quality quality, int degree)
    {
        bool IsValid(Quality quality, int degree) 
        {
            return (quality.IsPerfect() && Quality.IsPerfectDegree(degree))
                || (quality.IsImperfect() && !Quality.IsPerfectDegree(degree));
        }

        if (IsValid(quality, degree))
        {
            this.quality = quality;
            this.degree = degree;
        } else
            throw new ArgumentException($"Invalid quality/degree combination {quality}/{degree + 1}");
    }

    public Interval(Pitch p1, Pitch p2)
    {
        degree = DiatonicInterval(p1, p2);

        int defaultInterval = PitchName.ChromaticOffset(degree);
        int inflectedInterval = ChromaticInterval(p1, p2);
        int adjustment = inflectedInterval - defaultInterval;

        try 
        {
            quality = new Quality(degree, adjustment);
        }
        catch { throw; }
    }

    public static Interval? FromString(string input)
    {
        Interval? interval = null;

        void ThrowInputException() => 
            throw new Exception($"Could not create Interval from string {input}");

        if (input.Length >= 2)
        {
            Quality? quality = null;

            try
            {
                quality = new(input[0]); 
            }
            catch { throw; }
    
            int degree;
            bool test = Int32.TryParse(input[1..], out degree);
            if (quality != null && test)
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



    static int loopDiff(int n, int m, int max)
    {
        if (m < n)
            m += max;

        return m - n;
    }

    // TODO replace this and next with generics?
    static int DiatonicInterval(Pitch p1, Pitch p2)
    {
        int val1 = p1.DiatonicValue();
        int val2 = p2.DiatonicValue();
        return loopDiff(val1, val2, 7);
    }

    static int ChromaticInterval(Pitch p1, Pitch p2)
    {
        int val1 = p1.ChromaticValue();
        int val2 = p2.ChromaticValue();
        return loopDiff(val1, val2, 12);
    }


    public override string ToString()
    {
        return $"{quality}{degree + 1}";
    }
}

