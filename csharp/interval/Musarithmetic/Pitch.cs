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
 
    // To add an interval to a pitch, we get the letter name by adding the
    // diatonic values of the pitch (its offset) and the interval, modulo 7.
    // To get the accidental, we add the chromatic values of the pitch and the
    // interval (modulo 12) and then compare it with the chromatic value of
    // what the diatonic pitch from the previous step would be.
    //
    // Example: D + m2 = Eb
    //      1. D's diatonic offset is 1, diatonic value of m2 is 1; 1 + 1 = 2
    //        which is E.
    //      2. D's chromatic offset is 2, chromatic value of m2 is 1; 2 + 1 =
    //      3.
    //      3. E (result of step 1)'s chromatic value is 4. Compare to result
    //      of step 2: 3 - 4 = -1. An adjustment of -1 is a flat accidental.
    //      Result is Eb.
    Pitch Inc(Interval interval)
    {
        int newDiatonicOffset = DiatonicModulo(
                pname.DiatonicValue + interval.DiatonicShift);
        
        PitchName newPname = new(newDiatonicOffset);
        int diatonicBaseChromaticOffset = newPname.ChromaticValue;

        int startingOffset = this.ChromaticValue;
        int inflectedChromaticOffset = 
            ChromaticModulo(startingOffset + interval.ChromaticShift);

        int adjustment = CircularChromaticDiff(
                    inflectedChromaticOffset,
                    diatonicBaseChromaticOffset);

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
