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

    int Modulo(int numBase, int n)
    {
        if (n >= numBase) 
            n = n % numBase;
        if (n < 0)
            n = numBase + (n % numBase);
        return n;
    }

    int DiatonicModulo(int n)  => Modulo(7, n);
    int ChromaticModulo(int n) => Modulo(12, n);
    int ChromaticLoopDiff(int n, int m) => Interval.LoopDiff(n, m, 12);
  
    public Pitch Inc(Interval interval)
    {
        int newDiatonicOffset = DiatonicModulo(
                pname.DiatonicValue + interval.Degree);

        PitchName newPname = new(newDiatonicOffset);

        int startingOffset = this.ChromaticValue;

        int inflectedChromaticOffset = ChromaticModulo(
                startingOffset + interval.ChromaticValue);

        int diatonicBaseChromaticOffset = newPname.ChromaticValue;

        if (diatonicBaseChromaticOffset == 0 
                && diatonicBaseChromaticOffset < inflectedChromaticOffset)
            diatonicBaseChromaticOffset = 12;

        int adjustment = 
            inflectedChromaticOffset - diatonicBaseChromaticOffset;
       
        Accidental newAccid = new(adjustment);

        return new Pitch(newPname, newAccid);
    }

}
