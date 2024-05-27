namespace Musarithmetic;

public class Interval
{
    Quality quality;
    int degree;

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

