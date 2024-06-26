namespace Musarithmetic;

public class Pitch
{
    PitchName pname;
    Accidental accid;

    public Pitch(PitchName pname, Accidental accid)
    {
        this.pname = pname;
        this.accid = accid;
    }

    public Pitch(string pitchStr)
    {
        try
        {
            pname = new PitchName(pitchStr[..1]);
            accid = new Accidental(pitchStr[1..]);
        }
        catch { throw; }
    }

    public override string ToString() => $"{pname}{accid}";

    public int DiatonicValue { get => pname.DiatonicValue; }

    public int ChromaticValue { get => 
        pname.ChromaticValue + accid.Adjustment; }

    int Modulo(int numBase, int n) {
        int mod = n % numBase;
        return mod < 0 ? n + numBase : mod;
    }

    int DiatonicModulo(int n)  => Modulo(7, n);
    int ChromaticModulo(int n) => Modulo(12, n);

    int WithinMarginOfZero(int n, int modBase, int margin) =>
        modBase - n < margin ? n - modBase : n;

    int CircularChromaticDiff(int n, int m)
    {
        n = Modulo(12, n);
        m = Modulo(12, m);
        if (m == 0) n = WithinMarginOfZero(n, 12, 3);
        else if (n == 0) m = WithinMarginOfZero(m, 12, 3);
        
        return n - m;
    }
  
    Pitch Inc(Interval interval)
    {
        int newDiatonicOffset = DiatonicModulo(pname.DiatonicValue + interval.DiatonicShift);
        PitchName newPname = new(newDiatonicOffset);

        int startingOffset = this.ChromaticValue;
        int inflectedChromaticOffset = 
            ChromaticModulo(startingOffset + interval.ChromaticShift);

//        Console.WriteLine($"starting Offset: {startingOffset}, interval.ChromaticShift {interval.ChromaticShift}");

        int diatonicBaseChromaticOffset = newPname.ChromaticValue;

        int adjustment = CircularChromaticDiff(
                    inflectedChromaticOffset,
                    diatonicBaseChromaticOffset);

 //      Console.WriteLine($"inflectedChromaticOffset {inflectedChromaticOffset}, diatonicBaseChromaticOffset {diatonicBaseChromaticOffset}, adjustment {adjustment}");

        Accidental newAccid = new(adjustment);

        return new Pitch(newPname, newAccid);
    }

    public static Pitch operator + (Pitch pitch, Interval interval) => 
        pitch.Inc(interval);

    public static Pitch operator - (Pitch pitch, Interval interval) =>
        pitch.Inc(interval.Negate());

    public static Interval operator - (Pitch p1, Pitch p2)
        => new Interval(p1, p2);
}
