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
            pname = PitchNameHelper.FromString(pitchStr[..1]);
            accid = AccidentalHelper.FromString(pitchStr[1..]);
        }
        catch { throw; }
    }

    public override string ToString() => 
        pname.ToSymbol() + accid.ToSymbol();

    public int DiatonicValue() => pname.DiatonicOffset();

    public int ChromaticValue()
    {
        int offset = pname.ChromaticOffset();
        int adjustment = accid.Adjustment();
        return offset + adjustment;
    }

    static int Modulo(int numBase, int n)
    {
        if (n >= numBase) 
            n = n % numBase;
        if (n < 0)
            n = numBase + (n % numBase);
        return n;
    }

    static int DiatonicModulo(int n) => Modulo(7, n);
    static int ChromaticModulo(int n) => Modulo(12, n);
    static int ChromaticLoopDiff(int n, int m) =>
        Interval.LoopDiff(n, m, 12);
  
    public Pitch Inc(Interval interval)
    {
        int newDiatonicOffset = Pitch.DiatonicModulo(pname.DiatonicOffset() + interval.Degree);

        PitchName newPname = (PitchName)newDiatonicOffset;

        int startingOffset = this.ChromaticValue();

        int newChromaticOffset = ChromaticModulo(startingOffset + interval.ChromaticValue());

        int adjustment = newChromaticOffset - newPname.ChromaticOffset();

        Accidental newAccid = AccidentalHelper.FromAdjustment(adjustment);
        return new Pitch(newPname, newAccid);
    }

}
