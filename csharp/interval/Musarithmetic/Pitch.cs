namespace Musarithmetic;

public class Pitch
{
    PitchName pname;
    Accidental accid;

    public Pitch(string pitchStr)
    {
        try
        {
            pname = new PitchName(pitchStr[0]);
            accid = new Accidental(pitchStr[1..]);
        }
        catch { throw; }
    }

    public override string ToString()
    {
        return $"{this.pname}{this.accid}";
    }

    public int DiatonicValue() => pname.DiatonicOffset();

    public int ChromaticValue()
    {
        int offset = pname.ChromaticOffset();
        int adjustment = accid.Adjustment();
        return offset + adjustment;
    }

}
